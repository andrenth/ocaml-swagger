open Printf

let codegen ~path_base
            ~definition_base
            ~reference_base
            ~reference_root
            ?(output = stdout)
            ?module_name
            ~input () =
  let ic = open_in input in
  let s = really_input_string ic (in_channel_length ic) in
  let swagger = Swagger_j.swagger_of_string s in
  Gen.of_swagger
    ~path_base
    ~definition_base
    ~reference_base
    ~reference_root
    ?module_name
    swagger
  |> Gen.to_string
  |> fprintf output "%s\n%!"
