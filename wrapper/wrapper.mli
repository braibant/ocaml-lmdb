module C : sig
  include module type of Lmdb_bindings.C (Lmdb_generated)
end

(** [Not_enough_capacity n] is raised when the allocated output is too small.
   [n] is the required size. *)
exception Not_enough_capacity of int

module type Flags = sig
  type t

  val equal : t -> t -> bool

  val ( + ) : t -> t -> t

  val none : t
end

module Stat : sig
  type t =
    { psize: int
    ; depth: int
    ; branch_pages: int
    ; leaf_pages: int
    ; overflow_pages: int
    ; entries: int }
  [@@deriving sexp]
end

module Env : sig
  module Flags : sig
    include Flags

    val nosubdir : t
    (** no environment directory *)

    val nosync : t
    (** don't fsync after commit *)

    val rdonly : t
    (** read only *)

    val nometasync : t
    (** don't fsync metapage after commit *)

    val writemap : t
    (** use writable mmap *)

    val mapasync : t
    (** use asynchronous msync when #MDB_WRITEMAP is used *)

    val notls : t
    (** tie reader locktable slots to #MDB_txn objects instead of to threads *)

    val nolock : t
    (** don't do any locking, caller must manage their own locks *)

    val nordahead : t
    (** don't do readahead (no effect on Windows) *)

    val nomeminit : t
    (** don't initialize malloc'd memory before writing to datafile *)
  end

  type t

  val create :
       ?mapsize:int
       (** The size should be a multiple of the OS page size. The default is
       10485760 bytes. The size of the memory map is also the maximum size of
       the database. The value should be chosen as large as possible, to
       accommodate future growth of the database. *)
    -> ?maxreaders:int
       (** This defines the number of slots in the lock table that is used to track
       readers in the the environment. The default is 126.*)
    -> ?maxdbs:int
       (** Set the maximum number of named databases for the environment. This
       function is only needed if multiple databases will be used in the
       environment. Simpler applications that use the environment as a single
       unnamed database can ignore this option.*)
    -> ?flags:Flags.t
    -> ?mode:int
       (** The UNIX permissions to set on created files and semaphores. *)
    -> string
       (** The directory in which the database files reside. This directory must
       already exist and be writable. *)
    -> t

  val stat : t -> Stat.t
end

module Txn : sig
  type t

  val commit : t -> unit

  val abort : t -> unit

  val reset : t -> unit

  val renew : t -> unit

  val create : Env.t -> ?parent:t -> ?flags:Unsigned.uint -> unit -> t
end

module Dbi : sig
  (** Named database  *)
  type t

  val create : ?name:string -> ?flags:Unsigned.uint -> Txn.t -> t
end

module Input : sig
  type t

  val of_string : string -> t
  (** Copies the content of the string.  *)

  val of_int : int -> t

  val null : t
end

module Output : sig
  type 'a t

  val allocate_string : size_limit:int option -> String.t t

  val allocate_bytes : size_limit:int option -> Bytes.t t

  val allocate_bigstring : size_limit:int option -> Bigstring.t t

  val in_bytes_buffer : ?pos:int -> ?len:int -> Bytes.t -> unit t

  val in_bigstring_buffer : ?pos:int -> ?len:int -> Bigstring.t -> unit t
end

val put :
  Txn.t -> Dbi.t -> flags:Unsigned.uint -> key:Input.t -> data:Input.t -> unit

val get : Txn.t -> Dbi.t -> key:Input.t -> data:'a Output.t -> 'a

val delete : Txn.t -> Dbi.t -> key:Input.t -> data:Input.t -> unit

module Cursor : sig
  type _ t

  val run : Txn.t -> Dbi.t -> f:('a t -> 'b) -> 'b

  val first : 'a t -> unit

  val last : 'a t -> unit

  val next : 'a t -> unit

  val prev : 'a t -> unit

  val set : 'a t -> Input.t -> unit

  val close : 'a t -> unit

  val count : 'a t -> int

  (* val delete : 'a t -> options:unit -> unit *)

  val get_current : 'a t -> key:'a Output.t -> data:'b Output.t -> 'a * 'b
end
