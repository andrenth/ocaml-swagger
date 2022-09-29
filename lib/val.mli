module Sig : sig
  type t
  type param

  val labelled : ?descr:string -> string -> Ppxlib.Ast.core_type -> param
  val optional : ?descr:string -> string -> Ppxlib.Ast.core_type -> param
  val nolabel : Ppxlib.Ast.core_type -> param
  val constant : string -> t
  val pure : ?descr:string -> string -> param list -> Ppxlib.Ast.core_type -> t

  val field_setter :
    ?descr:string -> string -> param list -> Ppxlib.Ast.core_type -> t

  val http_request :
    ?descr:string -> string -> param list -> Ppxlib.Ast.core_type -> t

  val to_sig : t -> Ppxlib.Ast.value_description
end

module Impl : sig
  type t
  type param
  type origin
  type http_verb
  type return

  val labelled : ?origin:origin -> string -> Ppxlib.Ast.core_type -> param
  val optional : ?origin:origin -> string -> Ppxlib.Ast.core_type -> param
  val nolabel : string -> Ppxlib.Ast.core_type -> param
  val constant : string -> string -> t
  val identity : string -> param list -> t
  val record_constructor : string -> param list -> t
  val field_getter : string -> param list -> t
  val field_setter : string -> param list -> t
  val http_request : return:return -> http_verb -> string -> param list -> t
  val origin : Swagger_t.parameter -> origin
  val http_verb_of_string : string -> http_verb
  val module_ : string -> return
  val type_ : Ppxlib.Ast.core_type -> return
  val to_impl : t -> Ppxlib.Ast.structure_item
end

type t

val create : Sig.t -> Impl.t -> t
val signature : t -> Sig.t
val implementation : t -> Impl.t
