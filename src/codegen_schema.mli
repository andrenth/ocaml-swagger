type t

val create :
  reference_base:string -> reference_root:Mod.t -> Swagger_t.schema -> t

val kind_to_string : t -> string
val reference : t -> Swagger_t.reference option
val to_string : t -> string
