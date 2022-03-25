(** Representation of OCaml types in signatures and implementations. *)

(** {1 Signature types} *)

(** Type declarations in signatures. *)
module Sig : sig
  type t

  val abstract : ?descr:string -> string -> t
  (** Signature item for abstract types.

      [abstract ?descr ?is_record name] is a signature item for abstract types.
      The [descr] argument will be used to generate the documentation string,
      if provided. *)

  val unspecified : ?descr:string -> string -> t
  val to_sig : t -> Ppxlib.Ast.type_declaration
end

(** {1 Implementation types} *)

(** Type declarations in signatures. *)
module Impl : sig
  type t

  type record_field
  (** The type of record fields. *)

  val record_field :
    name:string ->
    orig_name:string ->
    type_:Ppxlib.Ast.core_type ->
    record_field

  (** Constructor function for record fields. *)

  val alias : ?int_or_string:bool -> string -> Ppxlib.Ast.core_type -> t
  (** Defines an alias type to be generated in an implementation.

      The optional [int_or_string] flag can be passed for aliases that can
      repersent either integers or strings. This is defined in the Swagger
      specification as a ["string"] type having an ["int-or-string"] format.
      The generated JSON conversion functions will attempt to produce an
      integer value if the string represents a valid integer. *)

  val record : string -> record_field list -> t
  val unspecified : string -> t
  val to_impl : t -> Ppxlib.Ast.structure
end

(** {1 Type declarations} *)

type t
(** The type used to represent signature and implementation types. *)

val create : Sig.t -> Impl.t -> t
(** Constructs a type representation from a signature and implementation types.

    The signature and implementation types are assumed to represent the same
    type in the generated code, according to OCaml's semantics. *)

val name : t -> string
(** The name of the type as defined in the signature type. *)

val signature : t -> Sig.t
(** The signature type representation. *)

val implementation : t -> Impl.t
(** The implementation type representation. *)
