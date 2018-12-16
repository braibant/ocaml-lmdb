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

  val of_int : int -> t
  val null : t
end

module Output  : sig
  type 'a t

  val allocate_bytes : Bytes.t t
  val allocate_bigstring : Bigstring.t t
  val bytes_buffer : ?pos: int -> ?len:int -> Bytes.t -> unit t
  val bigstring_buffer : ?pos: int -> ?len:int -> Bigstring.t -> unit t
end

val put :
  Txn.t -> Db.t -> flags:Unsigned.uint -> key:Input.t -> data:Input.t -> unit

val get : Txn.t -> Db.t -> key:Input.t -> data:'a Output.t -> 'a

val delete : Txn.t -> Db.t -> key:Input.t -> data:Input.t -> unit

module Cursor : sig
  type _ t
  val run : Txn.t -> Db.t -> f:('a t -> 'b) -> 'b

  val first : 'a t -> unit
  val last : 'a t -> unit
  val next : 'a t -> unit
  val prev : 'a t -> unit
  val set : 'a t -> Input.t -> unit

  val close : 'a t -> unit
  val count : 'a t -> int
  (* val delete : 'a t -> options:unit -> unit *)

  val get_current : 'a t -> key:'a Output.t -> data: 'b Output.t -> 'a * 'b

end
