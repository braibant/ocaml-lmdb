open Base
open! Ctypes
module C = Lmdb_bindings.C (Lmdb_generated)
open! C

exception Error of string * string

let raise_on_error ~here i =
  if i <> 0 then
    raise (Error (C.mdb_strerror i, Base.Source_code_position.to_string here))

let _is_error i = i <> 0

module Stat = struct
  type t =
    { psize: int
    ; depth: int
    ; branch_pages: int
    ; leaf_pages: int
    ; overflow_pages: int
    ; entries: int }
  [@@deriving sexp]

  let of_raw raw =
    { psize= Ctypes.getf raw C.Stat.ms_psize |> Unsigned.UInt.to_int
    ; depth= Ctypes.getf raw C.Stat.ms_depth |> Unsigned.UInt.to_int
    ; branch_pages=
        Ctypes.getf raw C.Stat.ms_branch_pages |> Unsigned.Size_t.to_int
    ; leaf_pages=
        Ctypes.getf raw C.Stat.ms_leaf_pages |> Unsigned.Size_t.to_int
    ; overflow_pages=
        Ctypes.getf raw C.Stat.ms_overflow_pages |> Unsigned.Size_t.to_int
    ; entries= Ctypes.getf raw C.Stat.ms_entries |> Unsigned.Size_t.to_int }
end

module Env = struct
  type t = {raw: Lmdb_types.Env.t ptr}

  let raw t = t.raw

  let create ?mapsize ?maxreaders ?maxdbs ?(flags = Unsigned.UInt.zero)
      ?(mode = 0o755) path =
    let env = Ctypes.allocate_n (ptr Lmdb_types.Env.t) ~count:1 in
    raise_on_error ~here:[%here] (C.mdb_env_create env) ;
    let env = !@env in
    ( try
        Option.iter mapsize ~f:(fun i ->
            C.Env.set_mapsize env (Unsigned.Size_t.of_int i)
            |> raise_on_error ~here:[%here] ) ;
        Option.iter maxreaders ~f:(fun i ->
            C.Env.set_maxreaders env (Unsigned.UInt.of_int i)
            |> raise_on_error ~here:[%here] ) ;
        Option.iter maxdbs ~f:(fun i ->
            C.Env.set_maxdbs env (Unsigned.UInt.of_int i)
            |> raise_on_error ~here:[%here] ) ;
        raise_on_error ~here:[%here] (C.Env.open_ env path flags mode) ;
        Caml.Gc.finalise Env.close env
      with e -> Env.close env ; raise e ) ;
    {raw= env}

  let stat t =
    let raw_stat = Ctypes.allocate_n C.Stat.t ~count:1 in
    raise_on_error ~here:[%here] (C.Env.stat (raw t) raw_stat) ;
    Stat.of_raw !@raw_stat
end

module Txn = struct
  type t = {raw: C.Txn.t ptr; mutable freed: bool}

  let raw t = t.raw

  let commit t =
    assert (not t.freed) ;
    raise_on_error ~here:[%here] (Txn.commit (raw t)) ;
    t.freed <- true

  let abort t =
    assert (not t.freed) ;
    Txn.abort (raw t) ;
    t.freed <- true

  let reset t =
    assert (not t.freed) ;
    Txn.reset (raw t)

  let renew t =
    assert (not t.freed) ;
    raise_on_error ~here:[%here] (Txn.renew (raw t))

  let create env ?parent ?(flags = Unsigned.UInt.zero) () =
    let parent =
      match parent with
      | None -> Ctypes.(from_voidp C.Txn.t null)
      | Some parent -> raw parent
    in
    let raw = Ctypes.allocate_n (ptr C.Txn.t) ~count:1 in
    raise_on_error ~here:[%here] (C.Txn.begin_ (Env.raw env) parent flags raw) ;
    {raw= !@raw; freed= false}
end

(* Returns a \0 terminated CArray that represents the content of s. *)
let char_array_of_string s =
  let p = Ctypes.CArray.make Ctypes.char (String.length s + 1) in
  for i = 0 to String.length s - 1 do
    Ctypes.CArray.set p i s.[i]
  done ;
  Ctypes.CArray.set p (String.length s) '\000' ;
  p

module Db = struct
  type t = {raw: C.Dbi.t; mutable closed: bool}

  let create ?name ?(flags = Unsigned.UInt.zero) txn =
    let dbi = Ctypes.allocate_n C.Dbi.t ~count:1 in
    let raw_txn = Txn.raw txn in
    (* The below is a bit baroque, but allows to pass the null ptr to dbi open.
       If this has the same effect as passing the empty string, then this should go away.*)
    let () =
      match name with
      | None ->
          raise_on_error ~here:[%here]
            (C.Dbi.open_ raw_txn Ctypes.(from_voidp char null) flags dbi)
      | Some s ->
          let a = char_array_of_string s in
          raise_on_error ~here:[%here]
            (C.Dbi.open_ raw_txn (Ctypes.CArray.start a) flags dbi)
    in
    {raw= !@dbi; closed= false}

  let raw t = t.raw
end

module Input = struct
  type t = String of {raw: C.Val.t; data: char Ctypes.CArray.t} | Null

  let of_string s =
    let raw = Ctypes.make C.Val.t in
    let data = char_array_of_string s in
    Ctypes.setf raw C.Val.mv_size
      (Unsigned.Size_t.of_int (Ctypes.CArray.length data)) ;
    Ctypes.setf raw C.Val.mv_data (Ctypes.to_voidp (Ctypes.CArray.start data)) ;
    String {raw; data}

  let null = Null

  let raw_addr = function
    | String {raw; _} -> Ctypes.addr raw
    | Null -> Ctypes.(from_voidp C.Val.t null)
end

module Output = struct
  type 'a t = To_string : C.Val.t -> string t

  let to_string () =
    let raw = Ctypes.make C.Val.t in
    To_string raw

  let raw_addr = function To_string v -> Ctypes.addr v

  let read (type a) (t : a t) =
    match t with To_string raw ->
      let length = Ctypes.getf raw C.Val.mv_size in
      let ptr = Ctypes.getf raw C.Val.mv_data in
      Ctypes.string_from_ptr (from_voidp char ptr)
        ~length:(Unsigned.Size_t.to_int length)
end

let put txn db ~flags ~key ~data =
  raise_on_error ~here:[%here]
    (C.put (Txn.raw txn) (Db.raw db) (Input.raw_addr key) (Input.raw_addr data)
       flags)

let get txn db ~key =
  let t = Output.to_string () in
  raise_on_error ~here:[%here]
    (C.get (Txn.raw txn) (Db.raw db) (Input.raw_addr key) (Output.raw_addr t)) ;
  Output.read t

let delete txn db ~key ~data =
  raise_on_error ~here:[%here]
    (C.del (Txn.raw txn) (Db.raw db) (Input.raw_addr key) (Input.raw_addr data))
