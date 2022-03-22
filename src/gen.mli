val of_swagger :
  ?path_base:string ->
  ?definition_base:string ->
  ?reference_base:string ->
  reference_root:string ->
  Swagger_t.swagger ->
  Mod.t

val to_string : Mod.t -> string
