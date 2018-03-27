module Sig : sig
  type t

  val abstract : ?descr:string -> string -> t
  val unspecified : ?descr:string -> string -> t

  val to_string : ?indent:int -> t -> string
end

module Impl : sig
  type t

  type record_field

  val alias : string -> string -> t
  val record : string -> record_field list -> t
  val unspecified : string -> t

  val record_field : name:string
                  -> orig_name:string
                  -> type_:string
                  -> record_field

  val to_string : ?indent:int -> t -> string
end

type t

val create : Sig.t -> Impl.t -> t
val name : t -> string

val signature : t -> Sig.t
val implementation : t -> Impl.t
