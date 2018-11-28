open Cmdliner
open! Lmdb.Wrapper

module Repl = struct
  let read () = read_line ()

  let prompt () = Stdio.printf "$ %!";

  module Expr = struct
    type t =
      | Nop
       | Read of string
       | Write of string * string
       | Begin_txn
       | Commit_txn
       | Abort_txn
  end

  module Parser = struct
    open Expr
    open Angstrom


    let parens p = char '(' *> p <* char ')'
    let alphanum = take_while (Base.Char.is_alphanum)
    let pair ~sep a b = a >>= fun a -> char sep *>  b >>= fun b -> return (a,b)

    let read = char 'r' *> parens alphanum >>= fun var -> return (Read var)
    let write =
      char 'w' *> parens (pair ~sep:',' alphanum alphanum) >>= fun (var,value) -> return (Write (var, value))

    let begin_ = string "begin" *> return Begin_txn
    let commit_ = string "commit" *> return Commit_txn
    let abort_ = string "abort" *> return Abort_txn

    let expr : Expr.t t =
      choice [read; write; begin_; commit_; abort_]

    let parse line = if String.length line = 0 then Ok Nop else parse_string expr line
  end

  module Eval = struct
    type t = {env : Env.t; mutable txns : Txn.t list; db : Db.t}
    let eval env (e : Expr.t) =
      match e with
      | Read var ->
        let result = get (List.hd env.txns) env.db ~key:(Input.of_string var) in
        Stdio.printf "> %s\n" result
      | Write (var, value) ->
        put (List.hd env.txns) env.db ~flags:Unsigned.UInt.zero ~key:(Input.of_string var)
          ~data:(Input.of_string value)
      | Begin_txn ->
        let txn = Txn.create env.env ~parent:(List.hd env.txns) () in
        env.txns <- txn :: env.txns
      | Commit_txn ->
        let txn = List.hd env.txns in
        Txn.commit txn;
        env.txns <- List.tl env.txns
      | Abort_txn ->
        let txn = List.hd env.txns in
        Txn.abort txn;
        env.txns <- List.tl env.txns
      | Nop ->
        Stdio.printf ""

    let create () =
      let env = Env.create ~flags:(Unsigned.UInt.of_int 0) ~mode:0o664 "/tmp/testdb" in
      let txn = Txn.create env () in
      let db = Db.create txn in
      {env; txns = [txn]; db}
  end

  let repl () =
    let env = Eval.create () in
    while true do
      prompt ();
      let line = read_line () in
      match  Parser.parse line  with
      | Result.Ok expr ->
        Eval.eval env expr
      | Error _ ->
        Stdio.printf "Error\n"
    done

end

let write ~key ~value =
  let env =
    Env.create ~flags:(Unsigned.UInt.of_int 0) ~mode:0o664 "/tmp/testdb"
  in
  let txn = Txn.create env () in
  let db = Db.create txn in
  let () =
    put txn db ~flags:Unsigned.UInt.zero ~key:(Input.of_string key)
      ~data:(Input.of_string value)
  in
  Txn.commit txn ; ()

let read ~key =
  let env =
    Env.create ~flags:(Unsigned.UInt.of_int 0) ~mode:0o664 "/tmp/testdb"
  in
  let txn = Txn.create env () in
  let db = Db.create txn in
  let result = get txn db ~key:(Input.of_string key) in
  Txn.commit txn ; Stdio.printf "%s\n" result

let stat () =
  let env =
    Env.create ~flags:(Unsigned.UInt.of_int 0) ~mode:0o664 "/tmp/testdb"
  in
  let result = Env.stat env in
  Stdio.printf !"%{sexp: Stat.t}\n" result

module Cmd = struct
  let key =
    Arg.(required & pos 0 (some string) None & info [] ~docv:"KEY" ~doc:"key")

  let value =
    Arg.(
      required & pos 1 (some string) None & info [] ~docv:"VAL" ~doc:"value")

  let write =
    let info = Cmdliner.Term.info "write" in
    let write key value = write ~key ~value in
    (Term.(const write $ key $ value), info)

  let read =
    let info = Cmdliner.Term.info "read" in
    let read key = read ~key in
    (Term.(const read $ key), info)

  let stat =
    let info = Cmdliner.Term.info "stat" in
    (Term.(const stat $ const ()), info)

  let repl =
    let info = Cmdliner.Term.info "repl" in
    (Term.(const Repl.repl $ const ()), info)
  let default =
    let doc = "a embedded database system" in
    let sdocs = Manpage.s_common_options in
    let exits = Term.default_exits in
    ( Term.(ret (const (`Help (`Pager, None))))
    , Term.info "single" ~doc ~sdocs ~exits ~man:[] )
end

let cmds = Cmd.[write; read; stat; repl]

let () = Term.(exit @@ eval_choice Cmd.default cmds)

(* nt main(int argc,char * argv[])
 * {
 * 	int rc;
 * 	MDB_env *env;
 * 	MDB_dbi dbi;
 * 	MDB_val key, data;
 * 	MDB_txn *txn;
 * 	MDB_cursor *cursor;
 * 	char sval[32];
 *
 * 	rc = mdb_env_create(&env);
 * 	rc = mdb_env_open(env, "./testdb", 0, 0664);
 * 	rc = mdb_txn_begin(env, NULL, 0, &txn);
 * 	rc = mdb_open(txn, NULL, 0, &dbi);
 *
 * 	key.mv_size = sizeof(int);
 * 	key.mv_data = sval;
 * 	data.mv_size = sizeof(sval);
 * 	data.mv_data = sval;
 *
 * 	sprintf(sval, "%03x %d foo bar", 32, 3141592);
 * 	rc = mdb_put(txn, dbi, &key, &data, 0);
 * 	rc = mdb_txn_commit(txn);
 * 	if (rc) {
 * 		fprintf(stderr, "mdb_txn_commit: (%d) %s\n", rc, mdb_strerror(rc));
 * 		goto leave;
 * 	}
 * 	rc = mdb_txn_begin(env, NULL, MDB_RDONLY, &txn);
 * 	rc = mdb_cursor_open(txn, dbi, &cursor);
 * 	while ((rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT)) == 0) {
 * 		printf("key: %p %.*s, data: %p %.*s\n",
 * 			key.mv_data,  (int) key.mv_size,  (char *\) key.mv_data,
 * 			data.mv_data, (int) data.mv_size, (char *\) data.mv_data);
 * 	}
 * 	mdb_cursor_close(cursor);
 * 	mdb_txn_abort(txn);
 * leave:
 * 	mdb_close(env, dbi);
 * 	mdb_env_close(env);
 * 	return 0;
 * } *)
