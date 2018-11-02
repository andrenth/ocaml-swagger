val of_swagger : ?path_base:string
              -> ?definition_base:string
              -> ?reference_base:string
              -> ?vendor_extension_plugin:(Swagger_j.swagger -> Mod.t -> Mod.t)
              -> reference_root:string
              -> io:[`Lwt | `Async]
              -> Swagger_t.swagger
              -> Mod.t

val to_string : io:[`Lwt | `Async] -> Mod.t -> string
