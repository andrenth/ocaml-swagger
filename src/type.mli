module Sig : sig
  type t

  val abstract : ?descr:string -> string -> t
  val unspecified : ?descr:string -> string -> t

  val to_string : ?indent:int -> t -> string
end

module Impl : sig
  type t

  type record_field

  val alias : ?int_or_string:bool -> string -> string -> t
  (** Defines an alias type to be generated in an implementation.

      The optional [int_or_string] flag can be passed for aliases that can
      repersent either integers or strings. This is defined in the Swagger
      specification as a ["string"] type having an ["int-or-string"] format.
      The generated JSON conversion functions will attempt to produce an
      integer value if the string repersents a valid integer. *)

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
