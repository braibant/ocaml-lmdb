open Base
open! Ctypes
module C = Lmdb_bindings.C (Lmdb_generated)
open! C

exception Error of string * string

exception Not_enough_capacity of int

let raise_on_error ~here i =
  if i <> 0 then
    raise (Error (C.mdb_strerror i, Base.Source_code_position.to_string here))

let raise_if_already_freed freed name =
  if freed then failwith (Printf.sprintf "%s used after being free" name)

let _is_error i = i <> 0

module type Flags = sig
  type t

  val equal : t -> t -> bool

  val ( + ) : t -> t -> t

  val none : t
end

module UIntFlags = struct
  type t = Unsigned.UInt.t

  let equal a b = Unsigned.UInt.compare a b = 0

  let ( + ) = Unsigned.UInt.logor

  let none = Unsigned.UInt.zero
end

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
  module Flags = struct
    open Lmdb_types
    include UIntFlags

    (* let fixedmap = _MDB_FIXEDMAP *)

    let nosubdir = _MDB_NOSUBDIR

    let nosync = _MDB_NOSYNC

    let rdonly = _MDB_RDONLY

    let nometasync = _MDB_NOMETASYNC

    let writemap = _MDB_WRITEMAP

    let mapasync = _MDB_MAPASYNC

    let notls = _MDB_NOTLS

    let nolock = _MDB_NOLOCK

    let nordahead = _MDB_NORDAHEAD

    let nomeminit = _MDB_NOMEMINIT
  end

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
    raise_if_already_freed t.freed "Txn" ;
    raise_on_error ~here:[%here] (Txn.commit (raw t)) ;
    t.freed <- true

  let abort t =
    raise_if_already_freed t.freed "Txn" ;
    Txn.abort (raw t) ;
    t.freed <- true

  let reset t =
    raise_if_already_freed t.freed "Txn" ;
    Txn.reset (raw t)

  let renew t =
    raise_if_already_freed t.freed "Txn" ;
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

module Dbi = struct
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
  type t =
    | String of {raw: C.Val.t; data: char Ctypes.CArray.t}
    | Null
    | Raw of {raw: C.Val.t; payload: unit Ctypes.ptr}

  let of_string s =
    let raw = Ctypes.make C.Val.t in
    let data = char_array_of_string s in
    Ctypes.setf raw C.Val.mv_size
      (Unsigned.Size_t.of_int (Ctypes.CArray.length data)) ;
    Ctypes.setf raw C.Val.mv_data (Ctypes.to_voidp (Ctypes.CArray.start data)) ;
    String {raw; data}

  let of_int i =
    let raw = Ctypes.make C.Val.t in
    let payload = Ctypes.to_voidp (Ctypes.allocate int i) in
    Ctypes.setf raw C.Val.mv_size (Unsigned.Size_t.of_int (sizeof int)) ;
    Ctypes.setf raw C.Val.mv_data payload ;
    Raw {raw; payload}

  let null = Null

  let raw_addr = function
    | String {raw; _} -> Ctypes.addr raw
    | Raw {raw; _} -> Ctypes.addr raw
    | Null -> Ctypes.(from_voidp C.Val.t null)
end

module Output = struct
  type 'a t =
    | Allocate_bytes : {size_limit: int option} -> Bytes.t t
    | Allocate_string : {size_limit: int option} -> String.t t
    | Allocate_bigstring : {size_limit: int option} -> Bigstring.t t
    | Write_to_bytes : {buffer: Bytes.t; pos: int; len: int} -> unit t
    | Write_to_bigstring : {buffer: Bigstring.t; pos: int; len: int} -> unit t

  let allocate_bytes ~size_limit = Allocate_bytes {size_limit}

  let allocate_string ~size_limit = Allocate_string {size_limit}

  let allocate_bigstring ~size_limit = Allocate_bigstring {size_limit}

  (* Capacity check is done in [prepare] *)
  let in_bigstring_buffer ?(pos = 0) ?len buffer =
    let len =
      match len with Some len -> len | None -> Bigstring.length buffer - pos
    in
    Write_to_bigstring {buffer; pos; len}

  let in_bytes_buffer ?(pos = 0) ?len buffer =
    let len =
      match len with Some len -> len | None -> Bytes.length buffer - pos
    in
    Write_to_bytes {buffer; pos; len}

  let check ~size_limit ~n =
    match size_limit with None -> true | Some len -> n <= len

  let has_capacity (type a) (t : a t) n =
    match t with
    | Allocate_bytes {size_limit} -> check ~size_limit ~n
    | Allocate_string {size_limit} -> check ~size_limit ~n
    | Allocate_bigstring {size_limit} -> check ~size_limit ~n
    | Write_to_bytes {len; _} -> n <= len
    | Write_to_bigstring {len; _} -> n <= len

  let copy (type a) (t : a t) v : a =
    let len = Ctypes.getf v C.Val.mv_size |> Unsigned.Size_t.to_int in
    if not (has_capacity t len) then raise (Not_enough_capacity len) ;
    let base = Ctypes.getf v C.Val.mv_data |> Ctypes.from_voidp char in
    match t with
    | Allocate_bytes {size_limit= _} ->
        let r = Bytes.create len in
        for i = 0 to len - 1 do
          Bytes.set r i Ctypes.(!@(base +@ i))
        done ;
        r
    | Allocate_string {size_limit= _} ->
        let r = Bytes.create len in
        for i = 0 to len - 1 do
          Bytes.set r i Ctypes.(!@(base +@ i))
        done ;
        Bytes.to_string r
    | _ -> failwith "Not implemented"

  (* let to_string () =
   *   let raw = Ctypes.make C.Val.t in
   *   To_string raw
   *
   * let raw_addr = function To_string v -> Ctypes.addr v
   *
   * let read (type a) (t : a t) =
   *   match t with To_string raw ->
   *     let length = Ctypes.getf raw C.Val.mv_size in
   *     let ptr = Ctypes.getf raw C.Val.mv_data in
   *     Ctypes.string_from_ptr (from_voidp char ptr)
   *       ~length:(Unsigned.Size_t.to_int length) *)
end

let put txn db ~flags ~key ~data =
  raise_on_error ~here:[%here]
    (C.put (Txn.raw txn) (Dbi.raw db) (Input.raw_addr key)
       (Input.raw_addr data) flags)

let get txn db ~key ~data:output =
  let data = Ctypes.make C.Val.t in
  raise_on_error ~here:[%here]
    (C.get (Txn.raw txn) (Dbi.raw db) (Input.raw_addr key) (Ctypes.addr data)) ;
  Output.copy output data

let delete txn db ~key ~data =
  raise_on_error ~here:[%here]
    (C.del (Txn.raw txn) (Dbi.raw db) (Input.raw_addr key)
       (Input.raw_addr data))

module Cursor = struct
  type _ t = {raw: C.Cursor.t ptr; key: C.Val.t; data: C.Val.t}

  let run txn dbi ~f =
    let cursor = Ctypes.allocate_n (ptr C.Cursor.t) ~count:1 in
    raise_on_error ~here:[%here]
      (C.Cursor.open_ (Txn.raw txn) (Dbi.raw dbi) cursor) ;
    let raw = !@cursor in
    let key = Ctypes.make C.Val.t in
    let data = Ctypes.make C.Val.t in
    let result = f {raw; key; data} in
    C.Cursor.close raw ; result

  let op0 cursor op =
    raise_on_error ~here:[%here]
      (C.Cursor.get cursor.raw (addr cursor.key) (addr cursor.data) op)

  let first cursor = op0 cursor Lmdb_types.MDB_FIRST

  let last cursor = op0 cursor Lmdb_types.MDB_LAST

  let next cursor = op0 cursor Lmdb_types.MDB_NEXT

  let prev cursor = op0 cursor Lmdb_types.MDB_PREV

  let set cursor input =
    let key = Input.raw_addr input in
    raise_on_error ~here:[%here]
      (C.Cursor.get cursor.raw key (addr cursor.data) Lmdb_types.MDB_SET)

  let get_current cursor ~key ~data =
    op0 cursor Lmdb_types.MDB_GET_CURRENT ;
    let k = Output.copy key cursor.key in
    let d = Output.copy data cursor.data in
    (k, d)

  let close cursor = C.Cursor.close cursor.raw

  let count cursor =
    let countp = allocate_n size_t ~count:1 in
    raise_on_error ~here:[%here] (C.Cursor.count cursor.raw countp) ;
    !@countp |> Unsigned.Size_t.to_int
end
