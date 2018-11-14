open Ctypes


module Abstract(X : sig val t : string end)() :  sig type t val t : t typ end =
struct
  type t = unit structure
  let t = typedef (structure X.t) X.t
end


module C(F: Cstubs.FOREIGN)() = struct
  open! F

  (* target version 0.9.22 *)

  module Env  = Abstract(struct let t = "MDB_env" end)()
  module Txn = Abstract(struct let t = "MDB_txn" end)()

  module Dbi : sig
    type t = Unsigned.UInt.t
    val t : t typ
  end
  =
  struct
    type t = Unsigned.UInt.t
    let t  = typedef (uint) "MDB_dbi"
  end


  module Cursor = Abstract(struct let t = "MDB_cursor" end)()

  module Val = struct
    type t
    let t : t typ = structure "MDB_val"
    let mv_size = field t "mv_size" size_t
    let mv_data = field t "mv_data" (ptr void)
    let _  = seal t
  end

  module Stat = struct

    type t
    let t : t typ = structure "MDB_stat"
    let ms_psize = field t "ms_psize" uint
    let ms_depth = field t "ms_depth" uint
    let ms_branch_pages = field t "ms_branch_pages" size_t
    let ms_leaf_pages = field t "ms_leaf_pages" size_t
    let ms_overflow_pages = field t "ms_overflow_pages" size_t
    let ms_entries = field t "ms_entries" size_t
    let _ = seal t
  end

  module Envinfo = struct
    type t
    let t : t typ = structure "MDB_envinfo"
 let me_mapaddr    = 	  field t     "me_mapaddr"       (ptr void)
 let me_mapsize    = 	  field t     "me_mapsize"       size_t
 let me_last_pgno  = 	  field t     "me_last_pgno"     size_t
 let me_last_txnid =    field t     "me_last_txnid"   	size_t
 let me_maxreaders =    field t     "me_maxreaders"    uint
 let me_numreaders	=   field t   	"me_numreaders"	 unt


  module Mode = struct
    type t = int
    let t : t typ = int_t
  end

  let or_error = returning int

  let mdb_version = foreign "mdb_version"
      (ptr int                  (*  major *)
       @-> ptr int              (* minor *)
       @-> ptr int              (* patch *)
       @-> returning (ptr char)
      )


  let mdb_strerror = foreign "mdb_strerror"
      (
        int                     (* err *)
        @-> returning (ptr char)
      )

  let mdb_env_create = foreign "mdb_env_create"
      (ptr (ptr Env.t)          (* env *)
       @-> or_error
      )

  let mdb_env_open = foreign "mdb_env_open"
      (
        ptr Env.t               (* env *)
        @-> ptr char            (* path *)
        @-> uint                (* flags *)
        @-> Mode.t              (* mode *)
        @-> or_error
      )

  let mdb_env_copy = foreign "mdb_env_open"
      (
        ptr Env.t               (* env *)
        @-> ptr char            (* path *)
        @-> or_error
      )

  (* mdb_env_copyfd *)
  (* mdb_env_copy2 *)
  (* mdb_env_copyfd2 *)

  let mdb_env_stat = foreign "mdb_env_stat"
      (
        ptr Env.t               (* env *)
        @-> ptr Stat.t          (* path *)
        @-> or_error
      )

  let mdb_env_info  = foreign "mdb_env_info"
      (
        ptr Env.t               (* env *)
        @-> ptr Envinfo.t          (* path *)
        @-> or_error
      )



end
