open! Ctypes
module C = Lmdb_bindings.C (Lmdb_generated)
open! C

exception Error of string

let raise_on_error i = if i <> 0 then raise (Error (C.mdb_strerror i))

let _is_error i = i <> 0

module Env = struct
  type t = {raw: C.Env.t ptr}

  let create ?(flags = Unsigned.UInt.zero) ?(mode = 0o755) path =
    let env = Ctypes.allocate_n (ptr C.Env.t) ~count:1 in
    raise_on_error (C.mdb_env_create env) ;
    let env = !@env in
    raise_on_error (C.Env.open_ env path flags mode) ;
    (* close? *)
    {raw= env}
end

module Txn = struct
  type t = {raw: C.Txn.t ptr}

  let raw t = t.raw

  module Experimental = struct
    let null = {raw= Ctypes.from_voidp C.Txn.t null}
  end
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
          raise_on_error
            (C.Dbi'.open_ raw_txn Ctypes.(from_voidp char null) flags dbi)
      | Some s ->
          let a = char_array_of_string s in
          raise_on_error
            (C.Dbi'.open_ raw_txn (Ctypes.CArray.start a) flags dbi)
    in
    {raw= !@dbi; closed= false}
end

(* module Transaction = struct
 *   type t = {
 *     env : Env.t;
 *     txn : C.Txn.t ptr;
 *     (\* parent? *\)
 *   }
 *
 *   let create ?(flags = Unsigned.UInt.zero) ?parent env db =
 *     let txn = Ctypes.allocate_n (ptr C.Txn.t) ~count:1 in
 *     raise_on_error (C.Txn'.begin_ (Env.raw env) db txn);
 *     let txn = !@ txn in
 *     {
 *       env;
 *       txn;
 *     }
 *
 * end *)
