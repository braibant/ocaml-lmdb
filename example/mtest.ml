open Base
open Stdio
open! Lmdb_core

let print_current_cursor cursor =
  let key, data =
    Cursor.get_current cursor ~key:(Output.allocate_string ~size_limit:None)
      ~data:(Output.allocate_string ~size_limit:None)
  in
  printf "key: %s, data: %s\n" key data

let main () =
  let count = Random.int 384 + 64 in
  let values = Array.init count ~f:(fun _i -> Random.int 1024) in
  let db = Sys.argv.(1) in
  let env =
    Env.create ~maxreaders:1 ~mapsize:10485760 ~flags:Lmdb_types._MDB_FIXEDMAP
      ~mode:0o664 db
  in
  let txn = Txn.create env () in
  let dbi = Dbi.create txn in
  let stats = Env.stat env in
  printf !"%{sexp: Stat.t}\n" stats ;
  printf "Adding %i values\n" count ;
  let j = ref 0 in
  for i = 0 to count - 1 do
    let key = Input.of_string (Printf.sprintf "%03x" values.(i)) in
    let data =
      Input.of_string (Printf.sprintf "%03x %d foo bar" values.(i) values.(i))
    in
    (* TODO: the error type should be reified here to allow to match on some return codes. *)
    try put txn dbi ~key ~data ~flags:Lmdb_types._MDB_NOOVERWRITE with _ ->
      Int.incr j
  done ;
  if !j > -0 then printf "%i duplicates skipped\n" !j ;
  Txn.commit txn ;
  let stats = Env.stat env in
  printf !"%{sexp: Stat.t}\n" stats ;
  let txn = Txn.create env ~flags:Lmdb_types._MDB_RDONLY () in
  Cursor.run txn dbi ~f:(fun cursor ->
      while try Cursor.next cursor ; true with _ -> false do
        print_current_cursor cursor
      done ) ;
  Txn.abort txn ;
  let i = ref (count - 1) in
  let j = ref 0 in
  while !i > -1 do
    Int.incr j ;
    let txn = Txn.create env () in
    let key = Input.of_string (Printf.sprintf "%03x" values.(!i)) in
    ( try
        delete txn dbi ~key ~data:Input.null ;
        Txn.commit txn
      with _ -> Int.decr j ; Txn.abort txn ) ;
    i := !i - Random.int 5
  done ;
  printf "Deleted %d values\n" !j ;
  let stats = Env.stat env in
  printf !"%{sexp: Stat.t}\n" stats ;
  let txn = Txn.create env ~flags:Lmdb_types._MDB_RDONLY () in
  Cursor.run txn dbi ~f:(fun cursor ->
      while try Cursor.next cursor ; true with _ -> false do
        print_current_cursor cursor
      done ;
      printf "Cursor last\n" ;
      Cursor.last cursor ;
      print_current_cursor cursor ;
      printf "Cursor prev\n" ;
      while try Cursor.prev cursor ; true with _ -> false do
        print_current_cursor cursor
      done ;
      printf "Cursor last/prev\n" ;
      Cursor.last cursor ;
      print_current_cursor cursor ;
      Cursor.prev cursor ;
      print_current_cursor cursor ) ;
  Txn.abort txn

;;
main ()
