open Ctypes

module C (F : Cstubs.FOREIGN) = struct
  open! F

  (* target version 0.9.22 *)

  module Env : sig type t val t : t typ end =  struct
    type t = unit structure

    let t = typedef (structure "MDB_env") "MDB_env"
  end

  module Txn : sig type t val t : t typ end =  struct
    type t = unit structure

    let t = typedef (structure "MDB_txn") "MDB_txn"
  end


  module Dbi : sig
    type t = Unsigned.UInt.t

    val t : t typ
  end = struct
    type t = Unsigned.UInt.t

    let t = typedef uint "MDB_dbi"
  end

  module Cursor : sig type t val t : t typ end =  struct
    type t = unit structure

    let t = typedef (structure "MDB_cursor") "MDB_cursor"
  end

  module Val = struct
    type t

    let t : t structure typ = structure "MDB_val"

    let mv_size = field t "mv_size" size_t

    let mv_data = field t "mv_data" (ptr void)

    let _ = seal t
  end

  module Stat = struct
    type t

    let t : t structure typ = structure "MDB_stat"

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

    let t : t structure  typ = structure "MDB_envinfo"

    let me_mapaddr = field t "me_mapaddr" (ptr void)

    let me_mapsize = field t "me_mapsize" size_t

    let me_last_pgno = field t "me_last_pgno" size_t

    let me_last_txnid = field t "me_last_txnid" size_t

    let me_maxreaders = field t "me_maxreaders" uint

    let me_numreaders = field t "me_numreaders" uint
  end

  module Mode = struct
    type t = int

    let t : t typ = int
  end

  let or_error = returning int

  let mdb_version =
    foreign "mdb_version"
      ( ptr int (*  major *)
      @-> ptr int (* minor *)
      @-> ptr int
      (* patch *)
      @-> returning (ptr char) )
  ;;

  let mdb_strerror =
    foreign "mdb_strerror" (int (* err *)
                           @-> returning string)
  ;;

  let mdb_env_create =
    foreign "mdb_env_create" (ptr (ptr Env.t) (* env *)
                             @-> or_error)
  ;;

  let mdb_env_open =
    foreign "mdb_env_open"
      ( ptr Env.t (* env *)
      @-> string (* path *)
      @-> uint (* flags *)
      @-> Mode.t (* mode *)
      @-> or_error )
  ;;

  let mdb_env_copy =
    foreign "mdb_env_copy"
      (ptr Env.t (* env *)
     @-> string (* path *)
     @-> or_error)
  ;;

  (* mdb_env_copyfd *)
  (* mdb_env_copy2 *)
  (* mdb_env_copyfd2 *)

  let mdb_env_stat =
    foreign "mdb_env_stat"
      (ptr Env.t (* env *)
     @-> ptr Stat.t
     @-> or_error)
  ;;

  let mdb_env_info =
    foreign "mdb_env_info"
      (ptr Env.t (* env *)
     @-> ptr Envinfo.t
     @-> or_error)
  ;;

  let mdb_env_sync =
    foreign "mdb_env_sync" (ptr Env.t (* env *)
                          @-> int (* force *)
                          @-> or_error)
  ;;

  let mdb_env_close =
    foreign "mdb_env_close" (ptr Env.t (* env *)
                           @-> returning void)
  ;;

  (* mdb_env_set_flags *)
  (* mdb_env_get_flags *)
  (* mdb_env_get_path *)
  (* mdb_env_get_ *)

  let mdb_env_set_mapsize =
    foreign "mdb_env_set_mapsize"
      (ptr Env.t (* env *)
     @-> size_t (* size *)
     @-> or_error)
  ;;

  let mdb_env_set_maxreaders =
    foreign "mdb_env_set_maxreaders"
      (ptr Env.t (* env *)
     @-> uint (* readers*)
     @-> or_error)
  ;;

  let mdb_env_get_maxreaders =
    foreign "mdb_env_get_maxreaders"
      (ptr Env.t (* env *)
     @-> ptr uint (* readers*)
     @-> or_error)
  ;;

  (* mdb_env_set_maxdbs *)

  let mdb_env_get_maxkeysize =
    foreign "mdb_env_get_maxkeysize" (ptr Env.t (* env *)
                                    @-> returning int)
  ;;

  (* mdb_env_set_userctx *)
  (* mdb_env_get_userctx *)

  (* typedef void MDB_assert_func(MDB_env *env, const char *msg) *)

  (* mdb_env_set_assert *)

  module Txn' = struct
    let begin_ =
      foreign "mdb_txn_begin"
        ( ptr Env.t (* env *)
        @-> ptr Txn.t (* parent *)
        @-> uint
        (* flags *)
        @-> ptr (ptr Txn.t)
        (* txn *)
        @-> or_error )
    ;;

    let env = foreign "mdb_txn_env" (ptr Txn.t @-> returning (ptr Env.t))

    let id = foreign "mdb_txn_id" (ptr Txn.t @-> returning size_t)

    let commit = foreign "mdb_txn_commit" (ptr Txn.t @-> returning int)

    let abort = foreign "mdb_txn_abort" (ptr Txn.t @-> returning void)

    let reset = foreign "mdb_txn_reset" (ptr Txn.t @-> returning void)

    let renew = foreign "mdb_txn_renew" (ptr Txn.t @-> returning int)
  end

  module Dbi' = struct
    let open_ =
      foreign "mdb_dbi_open"
        ( ptr Txn.t @-> ptr char (* name *)
        @-> uint (* flags *)
        @-> ptr Dbi.t (* dbi *)
        @-> or_error )
    ;;

    let stat =
      foreign "mdb_stat" (ptr Txn.t @-> Dbi.t @-> ptr Stat.t @-> or_error)
    ;;

    let flags =
      foreign "mdb_dbi_flags" (ptr Txn.t @-> Dbi.t @-> ptr uint @-> or_error)
    ;;

    let close = foreign "mdb_dbi_close" (ptr Env.t @-> Dbi.t @-> returning void)

    let drop = foreign "mdb_drop" (ptr Txn.t @-> Dbi.t @-> int @-> or_error)

    (* mdb_set_compare *)
    (* mdb_set_dupsort*)
    (* mdb_set_relfunc*)
    (* mdb_set_relctx*)
  end

  let get =
    foreign "mdb_get"
      ( ptr Txn.t (* txn *)
      @-> Dbi.t (* dbi *)
      @-> ptr Val.t (* key *)
      @-> ptr Val.t (* data *)
      @-> or_error )
  ;;

  let put =
    foreign "mdb_put"
      ( ptr Txn.t (* txn *)
      @-> Dbi.t (* dbi *)
      @-> ptr Val.t (* key *)
      @-> ptr Val.t (* data *)
      @-> uint (* flags *)
      @-> or_error )
  ;;

  let del =
    foreign "mdb_del"
      ( ptr Txn.t (* txn *)
      @-> Dbi.t (* dbi *)
      @-> ptr Val.t (* key *)
      @-> ptr Val.t (* data *)
      @-> or_error )
  ;;
end
