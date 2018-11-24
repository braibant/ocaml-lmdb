open! Ctypes
module C = Lmdb_bindings.C (Lmdb_generated)
open! C

exception Error of string

let raise_on_error i = if i <> 0 then raise (Error (C.mdb_strerror i))

let is_error i = i <> 0

module Env = struct
  type t = {env: C.Env.t ptr}

  let create ?(flags = Unsigned.UInt.zero) ?(mode = 0o755) path =
    let env = Ctypes.allocate_n (ptr C.Env.t) ~count:1 in
    raise_on_error (C.mdb_env_create env) ;
    let env = !@env in
    raise_on_error (C.Env.open_ env path flags mode) ;
    (* close? *)
    {env}
end
