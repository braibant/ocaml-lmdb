module C : sig
  include module type of Lmdb_bindings.C (Lmdb_generated)
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
    -> ?flags:Unsigned.uint
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

module Db : sig
  (** Named database  *)
  type t

  val create : ?name:string -> ?flags:Unsigned.uint -> Txn.t -> t
end

module Input : sig
  type t

  val of_string : string -> t
  (** Copies the content of the string.  *)

  val null : t
end

val put :
  Txn.t -> Db.t -> flags:Unsigned.uint -> key:Input.t -> data:Input.t -> unit

val get : Txn.t -> Db.t -> key:Input.t -> string

val delete : Txn.t -> Db.t -> key:Input.t -> data:Input.t -> unit
