module Validate = struct
  let version = (=) "2.0"
  let host _ = true
  let base_path _ =  true
  let url _ = true
  let email _ = true
  let path p = p.[0] = '/'
  let length ?(min = 0) ~max s =
    let len = String.length s in
    len > min && len < max
end

module Adapters = struct
  module Extensions : Atdgen_runtime.Json_adapter.S = struct
    (* val normalize : Yojson.Safe.json -> Yojson.Safe.json *)
    let normalize json =
      Yojson.Safe.Util.to_assoc json
      |> List.partition (fun (name, _) ->
           (String.length name >= 2) && (name.[0] = 'x' && name.[1] = '-'))
      |> (fun (extensions, rest) ->
           `Assoc (("_vendor_extensions", `Assoc extensions) :: rest))

    (* val restore : Yojson.Safe.json -> Yojson.Safe.json *)
    let restore json =
      Yojson.Safe.Util.to_assoc json
      |> List.partition (fun (name, _) ->
           String.equal name "_vendor_extensions")
      |> (fun (extensions, rest) ->
           match extensions with
           | [] -> `Assoc rest
           | [(_, x)] ->
             let assoc = Yojson.Safe.Util.to_assoc x in
             `Assoc (assoc @ rest)
           | (_ : (string * Yojson.Safe.json) list) ->
             failwith "invalid json: assoc list contains duplicates")

  end
end
