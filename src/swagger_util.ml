module Validate = struct
  let version = ( = ) "2.0"
  let host _ = true
  let base_path _ = true
  let url _ = true
  let email _ = true
  let path p = p.[0] = '/'

  let length ?(min = 0) ~max s =
    let len = String.length s in
    len > min && len < max
end
