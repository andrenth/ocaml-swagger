open Printf

let codegen ~path_base
            ~definition_base
            ~reference_base
            ~reference_root
            ~io
            ?(output = stdout)
            ?vendor_extension_plugin
            ~input =
  let ic = open_in input in
  let s = really_input_string ic (in_channel_length ic) in
  let swagger = Swagger_j.swagger_of_string s in
  Gen.of_swagger
    ~path_base
    ~definition_base
    ~reference_base
    ~reference_root
    ~io
    ?vendor_extension_plugin
    swagger
  |> Gen.to_string ~io
  |> fprintf output "%s\n%!"
