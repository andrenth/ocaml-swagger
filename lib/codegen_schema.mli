type t

val create :
  reference_base:string -> reference_root:Mod.t -> Swagger_t.schema -> t

val reference : t -> Swagger_t.reference option
val to_type : t -> Ppxlib.Ast.core_type
