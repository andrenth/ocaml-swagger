open Printf

module Swagger_j = Swagger_j
module Swagger_t = Swagger_t
module Gen       = Gen


let swagger_of_input ~input =
  let ic = open_in input in
  let s = really_input_string ic (in_channel_length ic) in
  Swagger_j.swagger_of_string s


let codegen_swagger_to_string ~path_base
                              ~definition_base
                              ~reference_base
                              ~reference_root
                              ~swagger =
  Gen.of_swagger
    ~path_base
    ~definition_base
    ~reference_base
    ~reference_root
    swagger
  |> Gen.to_string


let codegen ~path_base
            ~definition_base
            ~reference_base
            ~reference_root
            ?(output = stdout)
            ~input =
  let swagger = swagger_of_input ~input in
  codegen_swagger_to_string
    ~path_base
    ~definition_base
    ~reference_base
    ~reference_root
    ~swagger
  |> fprintf output "%s\n%!"
