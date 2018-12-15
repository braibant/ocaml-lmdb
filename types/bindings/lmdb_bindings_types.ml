open! Ctypes

module C (C : Cstubs.Types.TYPE) = struct
  open C

  module Helpers = struct
    let int x = C.constant x C.uint
  end

  open Helpers

  let _MDB_VERSION_MAJOR = int "MDB_VERSION_MAJOR"

  let _MDB_VERSION_MINOR = int "MDB_VERSION_MINOR"

  let _MDB_VERSION_PATCH = int "MDB_VERSION_PATCH"

  let _MDB_VERSION_FULL = int "MDB_VERSION_FULL"

  (* let _MDB_VERSION_DATE = C.constant "MDB_VERSION_DATE" C.(ptr char)
   *
   * let _MDB_VERSION_STRING = C.constant "MDB_VERSION_STRING" C.(ptr char) *)

  module Env = struct
    type phantom

    type t = phantom Ctypes_static.structure

    let t : phantom Ctypes_static.structure typ =
      typedef (structure "MDB_env") "MDB_env"
  end

  module Txn = struct
    type phantom

    type t = phantom Ctypes_static.structure

    let t : phantom Ctypes_static.structure typ =
      typedef (structure "MDB_txn") "MDB_txn"
  end

  module Dbi : sig
    type t

    val t : t typ
  end = struct
    type t = Unsigned.uint

    let t : t typ = typedef uint "MDB_dbi"
  end

  module Cursor = struct
    type phantom

    type t = phantom Ctypes_static.structure

    let t : phantom Ctypes_static.structure typ =
      typedef (structure "MDB_cursor") "MDB_cursor"
  end

  module Val = struct
    type phantom

    type t = phantom structure

    let t : t typ = structure "MDB_val"

    let mv_size = field t "mv_size" size_t

    let mv_data = field t "mv_data" (ptr void)

    let _ = seal t
  end

  (* /** @brief A callback function used to compare two keys in a database */
     typedef int  (MDB_cmp_func)(const MDB_val *a, const MDB_val *b);

     /** @brief A callback function used to relocate a position-dependent data item
   * in a fixed-address database.
   *
   * The \b newptr gives the item's desired address in
   * the memory map, and \b oldptr gives its previous address. The item's actual
   * data resides at the address in \b item.  This callback is expected to walk
   * through the fields of the record in \b item and modify any
   * values based at the \b oldptr address to be relative to the \b newptr address.
   * @param[in,out] item The item that is to be relocated.
   * @param[in] oldptr The previous address.
   * @param[in] newptr The new address to relocate to.
   * @param[in] relctx An application-provided context, set by #mdb_set_relctx().
   * @todo This feature is currently unimplemented.
   */
     typedef void (MDB_rel_func)(MDB_val *item, void *oldptr, void *newptr, void *relctx);
  *)

  (** Env flags  *)

  (** mmap at a fixed address (experimental) *)
  let _MDB_FIXEDMAP = int "MDB_FIXEDMAP"

  (** no environment directory *)
  let _MDB_NOSUBDIR = int "MDB_NOSUBDIR"

  (** don't fsync after commit *)
  let _MDB_NOSYNC = int "MDB_NOSYNC"

  (** read only *)
  let _MDB_RDONLY = int "MDB_RDONLY"

  (** don't fsync metapage after commit *)
  let _MDB_NOMETASYNC = int "MDB_NOMETASYNC"

  (** use writable mmap *)
  let _MDB_WRITEMAP = int "MDB_WRITEMAP"

  (** use asynchronous msync when #MDB_WRITEMAP is used *)
  let _MDB_MAPASYNC = int "MDB_MAPASYNC"

  (** tie reader locktable slots to #MDB_txn objects instead of to threads *)
  let _MDB_NOTLS = int "MDB_NOTLS"

  (** don't do any locking, caller must manage their own locks *)
  let _MDB_NOLOCK = int "MDB_NOLOCK"

  (** don't do readahead (no effect on Windows) *)
  let _MDB_NORDAHEAD = int "MDB_NORDAHEAD"

  (** don't initialize malloc'd memory before writing to datafile *)
  let _MDB_NOMEMINIT = int "MDB_NOMEMINIT"

  (* Database flags *)

  (** use reverse string keys *)
  let _MDB_REVERSEKEY = int "MDB_REVERSEKEY"

  (** use sorted duplicates *)
  let _MDB_DUPSORT = int "MDB_DUPSORT"

  (** numeric keys in native byte order: either unsigned int or size_t.
*  The keys must all be of the same size. *)
  let _MDB_INTEGERKEY = int "MDB_INTEGERKEY"

  (** with #MDB_DUPSORT, sorted dup items have fixed size *)
  let _MDB_DUPFIXED = int "MDB_DUPFIXED"

  (** with #MDB_DUPSORT, dups are #MDB_INTEGERKEY-style integers *)
  let _MDB_INTEGERDUP = int "MDB_INTEGERDUP"

  (** with #MDB_DUPSORT, use reverse string dups *)
  let _MDB_REVERSEDUP = int "MDB_REVERSEDUP"

  (** create DB if not already existing *)
  let _MDB_CREATE = int "MDB_CREATE"

  (* mdb_put	Write Flags *)

  (** For put: Don't write if the key already exists. *)
  let _MDB_NOOVERWRITE = int "MDB_NOOVERWRITE"

  (** Only for #MDB_DUPSORT<br>
  For put: don't write if the key and data pair already exist.<br>
  For mdb_cursor_del: remove all duplicate data items.
 *)
  let _MDB_NODUPDATA = int "MDB_NODUPDATA"

  (** For mdb_cursor_put: overwrite the current key/data pair *)
  let _MDB_CURRENT = int "MDB_CURRENT"

  (** For put: Just reserve space for data, don't copy it. Return a
  pointer to the reserved space.
*)

  let _MDB_RESERVE = int "MDB_RESERVE"

  (** Data is being appended, don't split full pages. *)
  let _MDB_APPEND = int "MDB_APPEND"

  (** Duplicate data is being appended, don't split full pages. *)
  let _MDB_APPENDDUP = int "MDB_APPENDDUP"

  (** Store multiple data items in one call. Only for #MDB_DUPFIXED. *)
  let _MDB_MULTIPLE = int "MDB_MULTIPLE"

  (*	mdb_copy	Copy Flags *)

  (** Compacting copy: Omit free space from copy, and renumber all
  pages sequentially.
 *)
  let _MDB_CP_COMPACT = int "MDB_CP_COMPACT"

  (** Cursor Get operations.

 	This is the set of all operations for retrieving data using a cursor. *)

  type cursor_op =
    | MDB_FIRST  (** Position at first key/data item *)
    | MDB_FIRST_DUP
        (** Position at first data item of current key.
                          Only for #MDB_DUPSORT *)
    | MDB_GET_BOTH  (** Position at key/data pair. Only for #MDB_DUPSORT *)
    | MDB_GET_BOTH_RANGE
        (** position at key nearest data. Only for #MDB_DUPSORT *)
    | MDB_GET_CURRENT  (** Return key/data at current cursor position *)
    | MDB_GET_MULTIPLE
        (** Return key and up to a page of duplicate data items
                            from current cursor position. Move cursor to prepare
                            for #MDB_NEXT_MULTIPLE. Only for #MDB_DUPFIXED *)
    | MDB_LAST  (** Position at last key/data item *)
    | MDB_LAST_DUP
        (** Position at last data item of current key.
                         Only for #MDB_DUPSORT *)
    | MDB_NEXT  (** Position at next data item *)
    | MDB_NEXT_DUP
        (** Position at next data item of current key. | Only
                           for #MDB_DUPSORT *)
    | MDB_NEXT_MULTIPLE
        (** Return key and up to a page of duplicate data
                             items from next cursor position. Move cursor to
                             prepare for #MDB_NEXT_MULTIPLE. Only for
                             #MDB_DUPFIXED *)
    | MDB_NEXT_NODUP  (** Position at first data item of next key *)
    | MDB_PREV  (** Position at previous data item *)
    | MDB_PREV_DUP
        (** Position at previous data item of current key. Only
                           for #MDB_DUPSORT *)
    | MDB_PREV_NODUP  (** Position at last data item of previous key *)
    | MDB_SET  (** Position at specified key *)
    | MDB_SET_KEY  (** Position at specified key, return key + data *)
    | MDB_SET_RANGE
        (** Position at first key greater than or equal to
                           specified key. *)
    | MDB_PREV_MULTIPLE
        (** Position at previous page and return key and up to a
                           page of duplicate data items. Only for #MDB_DUPFIXED
                           *)

  let _MDB_cursor_op =
    enum "MDB_cursor_op"
      [ (MDB_FIRST, constant "MDB_FIRST" int64_t)
      ; (MDB_FIRST_DUP, constant "MDB_FIRST_DUP" int64_t)
      ; (MDB_GET_BOTH, constant "MDB_GET_BOTH" int64_t)
      ; (MDB_GET_BOTH_RANGE, constant "MDB_GET_BOTH_RANGE" int64_t)
      ; (MDB_GET_CURRENT, constant "MDB_GET_CURRENT" int64_t)
      ; (MDB_GET_MULTIPLE, constant "MDB_GET_MULTIPLE" int64_t)
      ; (MDB_LAST, constant "MDB_LAST" int64_t)
      ; (MDB_LAST_DUP, constant "MDB_LAST_DUP" int64_t)
      ; (MDB_NEXT, constant "MDB_NEXT" int64_t)
      ; (MDB_NEXT_DUP, constant "MDB_NEXT_DUP" int64_t)
      ; (MDB_NEXT_MULTIPLE, constant "MDB_NEXT_MULTIPLE" int64_t)
      ; (MDB_NEXT_NODUP, constant "MDB_NEXT_NODUP" int64_t)
      ; (MDB_PREV, constant "MDB_PREV" int64_t)
      ; (MDB_PREV_DUP, constant "MDB_PREV_DUP" int64_t)
      ; (MDB_PREV_NODUP, constant "MDB_PREV_NODUP" int64_t)
      ; (MDB_SET, constant "MDB_SET" int64_t)
      ; (MDB_SET_KEY, constant "MDB_SET_KEY" int64_t)
      ; (MDB_SET_RANGE, constant "MDB_SET_RANGE" int64_t)
      ; (MDB_PREV_MULTIPLE, constant "MDB_PREV_MULTIPLE" int64_t) ]

  (*
/** @defgroup  errors	Return Codes
 *
 *	BerkeleyDB uses -30800 to -30999, we'll go under them
 *	@{
 */
/**	Successful result */
#define MDB_SUCCESS	 0
/** key/data pair already exists */
#define MDB_KEYEXIST	(-30799)
/** key/data pair not found (EOF) */
#define MDB_NOTFOUND	(-30798)
/** Requested page not found - this usually indicates corruption */
#define MDB_PAGE_NOTFOUND	(-30797)
/** Located page was wrong type */
#define MDB_CORRUPTED	(-30796)
/** Update of meta page failed or environment had fatal error */
#define MDB_PANIC		(-30795)
/** Environment version mismatch */
#define MDB_VERSION_MISMATCH	(-30794)
/** File is not a valid LMDB file */
#define MDB_INVALID	(-30793)
/** Environment mapsize reached */
#define MDB_MAP_FULL	(-30792)
/** Environment maxdbs reached */
#define MDB_DBS_FULL	(-30791)
/** Environment maxreaders reached */
#define MDB_READERS_FULL	(-30790)
/** Too many TLS keys in use - Windows only */
#define MDB_TLS_FULL	(-30789)
/** Txn has too many dirty pages */
#define MDB_TXN_FULL	(-30788)
/** Cursor stack too deep - internal error */
#define MDB_CURSOR_FULL	(-30787)
/** Page has not enough space - internal error */
#define MDB_PAGE_FULL	(-30786)
/** Database contents grew beyond environment mapsize */
#define MDB_MAP_RESIZED	(-30785)
/** Operation and DB incompatible, or DB type changed. This can mean:
*	<ul>
*	<li>The operation expects an #MDB_DUPSORT / #MDB_DUPFIXED database.
*	<li>Opening a named DB when the unnamed DB has #MDB_DUPSORT / #MDB_INTEGERKEY.
*	<li>Accessing a data record as a database, or vice versa.
*	<li>The database was dropped and recreated with different flags.
*	</ul>
*/
#define MDB_INCOMPATIBLE	(-30784)
/** Invalid reuse of reader locktable slot */
#define MDB_BAD_RSLOT		(-30783)
/** Transaction must abort, has a child, or is invalid */
#define MDB_BAD_TXN			(-30782)
/** Unsupported size of key/DB name/data, or wrong DUPFIXED size */
#define MDB_BAD_VALSIZE		(-30781)
/** The specified DBI was changed unexpectedly */
#define MDB_BAD_DBI		(-30780)
/** The last defined error code */
#define MDB_LAST_ERRCODE	MDB_BAD_DBI
/** @} */
 *)
  module Stat = struct
    type t

    let t : t structure typ = structure "MDB_stat"

    (** Size of a database page. This is currently the same for all databases.
       *)
    let ms_psize = field t "ms_psize" uint

    (**  Depth (height) of the B-tree   *)
    let ms_depth = field t "ms_depth" uint

    (**  Number of internal (non-leaf) pages *)
    let ms_branch_pages = field t "ms_branch_pages" size_t

    (**  Number of leaf pages *)
    let ms_leaf_pages = field t "ms_leaf_pages" size_t

    (** Number of overflow pages  *)
    let ms_overflow_pages = field t "ms_overflow_pages" size_t

    (**Number of data items  *)
    let ms_entries = field t "ms_entries" size_t

    let _ = seal t
  end

  module Envinfo = struct
    type t

    let t : t structure typ = structure "MDB_envinfo"

    (** Address of map, if fixed  *)
    let me_mapaddr = field t "me_mapaddr" (ptr void)

    (** Size of the data memory map  *)
    let me_mapsize = field t "me_mapsize" size_t

    (** ID of the last used page  *)
    let me_last_pgno = field t "me_last_pgno" size_t

    (** ID of the last committed transaction  *)
    let me_last_txnid = field t "me_last_txnid" size_t

    (** max reader slots in the environment  *)
    let me_maxreaders = field t "me_maxreaders" uint

    (** max reader slots used in the environment  *)
    let me_numreaders = field t "me_numreaders" uint

    let _ = seal t
  end
end

(*

/** @brief Compare two data items according to a particular database.
*
* This returns a comparison as if the two data items were keys in the
* specified database.
* @param[in] txn A transaction handle returned by #mdb_txn_begin()
* @param[in] dbi A database handle returned by #mdb_dbi_open()
* @param[in] a The first item to compare
* @param[in] b The second item to compare
* @return < 0 if a < b, 0 if a == b, > 0 if a > b
*/
int  mdb_cmp(MDB_txn *txn, MDB_dbi dbi, const MDB_val *a, const MDB_val *b);

/** @brief Compare two data items according to a particular database.
*
* This returns a comparison as if the two items were data items of
* the specified database. The database must have the #MDB_DUPSORT flag.
* @param[in] txn A transaction handle returned by #mdb_txn_begin()
* @param[in] dbi A database handle returned by #mdb_dbi_open()
* @param[in] a The first item to compare
* @param[in] b The second item to compare
* @return < 0 if a < b, 0 if a == b, > 0 if a > b
*/
int  mdb_dcmp(MDB_txn *txn, MDB_dbi dbi, const MDB_val *a, const MDB_val *b);

/** @brief A callback function used to print a message from the library.
*
* @param[in] msg The string to be printed.
* @param[in] ctx An arbitrary context pointer for the callback.
* @return < 0 on failure, >= 0 on success.
*/
typedef int (MDB_msg_func)(const char *msg, void *ctx);

/** @brief Dump the entries in the reader lock table.
*
* @param[in] env An environment handle returned by #mdb_env_create()
* @param[in] func A #MDB_msg_func function
* @param[in] ctx Anything the message function needs
* @return < 0 on failure, >= 0 on success.
*/
int	mdb_reader_list(MDB_env *env, MDB_msg_func *func, void *ctx);

/** @brief Check for stale entries in the reader lock table.
*
* @param[in] env An environment handle returned by #mdb_env_create()
* @param[out] dead Number of stale slots that were cleared
* @return 0 on success, non-zero on failure.
*/
int	mdb_reader_check(MDB_env *env, int *dead);
/**	@} */


*)
