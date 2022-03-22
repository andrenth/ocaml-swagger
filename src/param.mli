type t = Swagger_t.parameter

val create :
  ?duplicate:bool ->
  reference_base:string ->
  reference_root:Mod.t ->
  t ->
  Val.Sig.param * Val.Impl.param

val name : string -> string
val kind_to_string : t -> string
