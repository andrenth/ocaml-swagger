open Printf

let () =
  let ic = open_in "src/swagger.json" in
  let s = really_input_string ic (in_channel_length ic) in
  let swagger = Swagger_j.swagger_of_string s in
  let signature =
    Gen.of_swagger
      ~path_base:"/api/"
      ~definition_base:"io.k8s."
      ~reference_base:"#/definitions/io.k8s."
      swagger in
  printf "%s\n%!" (signature |> Gen.to_string)
