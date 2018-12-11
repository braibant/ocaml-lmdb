open Ctypes
module Types = Lmdb_types

module C (F : Cstubs.FOREIGN) = struct
  open! F

  let or_error = returning int

  (* target version 0.9.22 *)
  module Mode = struct
    type t = int

    let t : t typ = int
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

    let t : t structure typ = structure "MDB_envinfo"

    let me_mapaddr = field t "me_mapaddr" (ptr void)

    let me_mapsize = field t "me_mapsize" size_t

    let me_last_pgno = field t "me_last_pgno" size_t

    let me_last_txnid = field t "me_last_txnid" size_t

    let me_maxreaders = field t "me_maxreaders" uint

    let me_numreaders = field t "me_numreaders" uint

    let _ = seal t
  end

  module Env = struct
    open Types.Env

    let open_ =
      foreign "mdb_env_open"
        ( ptr t (* env *)
        @-> string (* path *)
        @-> uint (* flags *)
        @-> Mode.t (* mode *)
        @-> or_error )

    let copy =
      foreign "mdb_env_copy" (ptr t (* env *)
                            @-> string (* path *)
                            @-> or_error)

    (* mdb_env_copyfd *)
    (* mdb_env_copy2 *)
    (* mdb_env_copyfd2 *)

    let stat =
      foreign "mdb_env_stat" (ptr t (* env *)
                            @-> ptr Stat.t @-> or_error)

    let info =
      foreign "mdb_env_info" (ptr t (* env *)
                            @-> ptr Envinfo.t @-> or_error)

    let sync =
      foreign "mdb_env_sync" (ptr t (* env *)
                            @-> int (* force *)
                            @-> or_error)

    let close = foreign "mdb_env_close" (ptr t (* env *)
                                       @-> returning void)

    (* mdb_env_set_flags *)
    (* mdb_env_get_flags *)
    (* mdb_env_get_path *)
    (* mdb_env_get_ *)

    let set_mapsize =
      foreign "mdb_env_set_mapsize"
        (ptr t (* env *)
       @-> size_t (* size *)
       @-> or_error)

    let set_maxreaders =
      foreign "mdb_env_set_maxreaders"
        (ptr t (* env *)
       @-> uint (* readers*)
       @-> or_error)

    let get_maxreaders =
      foreign "mdb_env_get_maxreaders"
        (ptr t (* env *)
       @-> ptr uint (* readers*)
       @-> or_error)

    let set_maxdbs =
      foreign "mdb_env_set_maxdbs"
        (ptr t (* env *)
       @-> uint (* dbs*)
       @-> or_error)

    (* mdb_env_set_maxdbs *)

    let get_maxkeysize =
      foreign "mdb_env_get_maxkeysize" (ptr t (* env *)
                                      @-> returning int)

    (* mdb_env_set_userctx *)
    (* mdb_env_get_userctx *)
    
    (* typedef void MDB_assert_func(MDB_env *env, const char *msg) *)
    
    (* mdb_env_set_assert *)
  end

  let mdb_version =
    foreign "mdb_version"
      ( ptr int (*  major *)
      @-> ptr int (* minor *)
      @-> ptr int
      (* patch *)
      @-> returning (ptr char) )

  let mdb_strerror = foreign "mdb_strerror" (int (* err *)
                                           @-> returning string)

  let mdb_env_create =
    foreign "mdb_env_create" (ptr (ptr Types.Env.t) (* env *)
                             @-> or_error)

  module Val = Types.Val

  module Txn = struct
    include Types.Txn

    let begin_ =
      foreign "mdb_txn_begin"
        ( ptr Types.Env.t (* env *)
        @-> ptr t (* parent *)
        @-> uint
        (* flags *)
        @-> ptr (ptr t)
        (* txn *)
        @-> or_error )

    let env = foreign "mdb_txn_env" (ptr t @-> returning (ptr Types.Env.t))

    let id = foreign "mdb_txn_id" (ptr t @-> returning size_t)

    let commit = foreign "mdb_txn_commit" (ptr t @-> returning int)

    let abort = foreign "mdb_txn_abort" (ptr t @-> returning void)

    let reset = foreign "mdb_txn_reset" (ptr t @-> returning void)

    let renew = foreign "mdb_txn_renew" (ptr t @-> returning int)
  end

  module Dbi = struct
    include Types.Dbi

    let open_ =
      foreign "mdb_dbi_open"
        ( ptr Txn.t @-> ptr char (* name *)
        @-> uint (* flags *)
        @-> ptr t (* dbi *)
        @-> or_error )

    let stat = foreign "mdb_stat" (ptr Txn.t @-> t @-> ptr Stat.t @-> or_error)

    let flags =
      foreign "mdb_dbi_flags" (ptr Txn.t @-> t @-> ptr uint @-> or_error)

    let close =
      foreign "mdb_dbi_close" (ptr Types.Env.t @-> t @-> returning void)

    let drop = foreign "mdb_drop" (ptr Txn.t @-> t @-> int @-> or_error)

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

  let put =
    foreign "mdb_put"
      ( ptr Txn.t (* txn *)
      @-> Dbi.t (* dbi *)
      @-> ptr Val.t (* key *)
      @-> ptr Val.t (* data *)
      @-> uint (* flags *)
      @-> or_error )

  let del =
    foreign "mdb_del"
      ( ptr Txn.t (* txn *)
      @-> Dbi.t (* dbi *)
      @-> ptr Val.t (* key *)
      @-> ptr Val.t (* data *)
      @-> or_error )
end
