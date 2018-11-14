open Ctypes


module C(F: Cstubs.FOREIGN) = struct
  open! F

  module Mdb_env : sig
    type t
    val t : t typ
  end
  =
  struct
    type t = unit structure
    let t = typedef (structure "MDB_env") "MDB_env"
  end

  module Mdb_txn : sig
    type t
    val t : t typ
  end
  =
  struct
    type t = unit structure
    let t = typedef (structure "MDB_txn") "MDB_txn"
  end

  module Mdb_dbi : sig
    type t = Unsigned.UInt.t
    val t : t typ
  end
  =
  struct
    type t = Unsigned.UInt.t
    let t  = typedef (uint) "MDB_dbi"
  end

  let mdb_env_create = foreign "mdb_env_create" (ptr (ptr Mdb_env.t) @-> returning int)

end
