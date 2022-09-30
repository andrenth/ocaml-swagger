module Object = struct
  module type Value = sig
    type value

    val value_of_yojson : Yojson.Safe.t -> value
    val yojson_of_value : value -> Yojson.Safe.t
  end

  module type S = sig
    type value
    type t = (string * value) list [@@deriving yojson]
  end

  module Make (V : Value) : S with type value := V.value = struct
    type t = (string * V.value) list [@@deriving yojson]

    let yojson_of_t obj =
      `Assoc (List.map (fun (k, v) -> (k, V.yojson_of_value v)) obj)

    let t_of_yojson (obj : Yojson.Safe.t) : t =
      let rec loop acc = function
        | [] -> List.rev acc
        | (k, v) :: obj ->
            let v = V.value_of_yojson v in
            loop ((k, v) :: acc) obj
      in
      match obj with
      | `Assoc obj -> loop [] obj
      | _ -> invalid_arg "invalid object"
  end

  module Of_strings = Make (struct
    type value = string [@@deriving yojson]
  end)

  module Of_floats = Make (struct
    type value = float [@@deriving yojson]
  end)

  module Of_ints = Make (struct
    type value = int [@@deriving yojson]
  end)

  module Of_ints32 = Make (struct
    type value = int32 [@@deriving yojson]
  end)

  module Of_ints64 = Make (struct
    type value = int64 [@@deriving yojson]
  end)

  module Of_bools = Make (struct
    type value = bool [@@deriving yojson]
  end)
end

module rec Swagger_petstore : sig
  module Definitions : sig
    module Api_response : sig
      type t [@@deriving yojson]

      val make : ?message:string -> ?type_:string -> ?code:int32 -> unit -> t
      val code : t -> int32 option

      val set_code : int32 option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the code field.|ocamlswagger}]

      val type_ : t -> string option

      val set_type_ : string option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the type_ field.|ocamlswagger}]

      val message : t -> string option

      val set_message : string option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the message field.|ocamlswagger}]

      module Object : Object.S with type value := t
    end

    module Category : sig
      type t [@@deriving yojson]

      val make : ?name:string -> ?id:int64 -> unit -> t
      val id : t -> int64 option

      val set_id : int64 option -> t -> t
        [@@ocaml.doc {ocamlswagger|Set the value of the id field.|ocamlswagger}]

      val name : t -> string option

      val set_name : string option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the name field.|ocamlswagger}]

      module Object : Object.S with type value := t
    end

    module Order : sig
      type t [@@deriving yojson]

      val make :
        ?complete:bool ->
        ?status:string ->
        ?ship_date:float ->
        ?quantity:int32 ->
        ?pet_id:int64 ->
        ?id:int64 ->
        unit ->
        t

      val id : t -> int64 option

      val set_id : int64 option -> t -> t
        [@@ocaml.doc {ocamlswagger|Set the value of the id field.|ocamlswagger}]

      val pet_id : t -> int64 option

      val set_pet_id : int64 option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the pet_id field.|ocamlswagger}]

      val quantity : t -> int32 option

      val set_quantity : int32 option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the quantity field.|ocamlswagger}]

      val ship_date : t -> float option

      val set_ship_date : float option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the ship_date field.|ocamlswagger}]

      val status : t -> string option
        [@@ocaml.doc {ocamlswagger|Order Status|ocamlswagger}]

      val set_status : string option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the status field.|ocamlswagger}]

      val complete : t -> bool option

      val set_complete : bool option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the complete field.|ocamlswagger}]

      module Object : Object.S with type value := t
    end

    module Pet : sig
      type t [@@deriving yojson]

      val make :
        ?status:string ->
        ?tags:Swagger_petstore.Definitions.Tag.t list ->
        photo_urls:string list ->
        name:string ->
        ?category:Swagger_petstore.Definitions.Category.t ->
        ?id:int64 ->
        unit ->
        t

      val id : t -> int64 option

      val set_id : int64 option -> t -> t
        [@@ocaml.doc {ocamlswagger|Set the value of the id field.|ocamlswagger}]

      val category : t -> Swagger_petstore.Definitions.Category.t option

      val set_category :
        Swagger_petstore.Definitions.Category.t option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the category field.|ocamlswagger}]

      val name : t -> string

      val set_name : string -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the name field.|ocamlswagger}]

      val photo_urls : t -> string list

      val set_photo_urls : string list -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the photo_urls field.|ocamlswagger}]

      val tags : t -> Swagger_petstore.Definitions.Tag.t list option

      val set_tags : Swagger_petstore.Definitions.Tag.t list option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the tags field.|ocamlswagger}]

      val status : t -> string option
        [@@ocaml.doc {ocamlswagger|pet status in the store|ocamlswagger}]

      val set_status : string option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the status field.|ocamlswagger}]

      module Object : Object.S with type value := t
    end

    module Tag : sig
      type t [@@deriving yojson]

      val make : ?name:string -> ?id:int64 -> unit -> t
      val id : t -> int64 option

      val set_id : int64 option -> t -> t
        [@@ocaml.doc {ocamlswagger|Set the value of the id field.|ocamlswagger}]

      val name : t -> string option

      val set_name : string option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the name field.|ocamlswagger}]

      module Object : Object.S with type value := t
    end

    module User : sig
      type t [@@deriving yojson]

      val make :
        ?user_status:int32 ->
        ?phone:string ->
        ?password:string ->
        ?email:string ->
        ?last_name:string ->
        ?first_name:string ->
        ?username:string ->
        ?id:int64 ->
        unit ->
        t

      val id : t -> int64 option

      val set_id : int64 option -> t -> t
        [@@ocaml.doc {ocamlswagger|Set the value of the id field.|ocamlswagger}]

      val username : t -> string option

      val set_username : string option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the username field.|ocamlswagger}]

      val first_name : t -> string option

      val set_first_name : string option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the first_name field.|ocamlswagger}]

      val last_name : t -> string option

      val set_last_name : string option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the last_name field.|ocamlswagger}]

      val email : t -> string option

      val set_email : string option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the email field.|ocamlswagger}]

      val password : t -> string option

      val set_password : string option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the password field.|ocamlswagger}]

      val phone : t -> string option

      val set_phone : string option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the phone field.|ocamlswagger}]

      val user_status : t -> int32 option
        [@@ocaml.doc {ocamlswagger|User Status|ocamlswagger}]

      val set_user_status : int32 option -> t -> t
        [@@ocaml.doc
          {ocamlswagger|Set the value of the user_status field.|ocamlswagger}]

      module Object : Object.S with type value := t
    end
  end

  module Pet : sig
    module Find_by_status : sig
      val request_path_template : unit -> string

      val get :
        status:string array ->
        ?ctx:Cohttp_lwt_unix.Client.ctx ->
        ?headers:Cohttp.Header.t ->
        Uri.t ->
        (Swagger_petstore.Definitions.Pet.t list, string) result Lwt.t
        [@@ocaml.doc
          {ocamlswagger|Multiple status values can be provided with comma separated strings
@param status Status values that need to be considered for filter|ocamlswagger}]
    end

    module Find_by_tags : sig
      val request_path_template : unit -> string

      val get :
        tags:string array ->
        ?ctx:Cohttp_lwt_unix.Client.ctx ->
        ?headers:Cohttp.Header.t ->
        Uri.t ->
        (Swagger_petstore.Definitions.Pet.t list, string) result Lwt.t
        [@@ocaml.doc
          {ocamlswagger|Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.
@param tags Tags to filter by|ocamlswagger}]
    end

    val request_path_template : unit -> string

    val put :
      body:Swagger_petstore.Definitions.Pet.t ->
      ?ctx:Cohttp_lwt_unix.Client.ctx ->
      ?headers:Cohttp.Header.t ->
      Uri.t ->
      (unit, string) result Lwt.t
      [@@ocaml.doc
        {ocamlswagger|
@param body Pet object that needs to be added to the store|ocamlswagger}]

    val post :
      body:Swagger_petstore.Definitions.Pet.t ->
      ?ctx:Cohttp_lwt_unix.Client.ctx ->
      ?headers:Cohttp.Header.t ->
      Uri.t ->
      (unit, string) result Lwt.t
      [@@ocaml.doc
        {ocamlswagger|
@param body Pet object that needs to be added to the store|ocamlswagger}]
  end

  module Store : sig
    module Inventory : sig
      val request_path_template : unit -> string

      val get :
        ?ctx:Cohttp_lwt_unix.Client.ctx ->
        ?headers:Cohttp.Header.t ->
        Uri.t ->
        (Object.Of_ints32.t, string) result Lwt.t
        [@@ocaml.doc
          {ocamlswagger|Returns a map of status codes to quantities|ocamlswagger}]
    end

    module Order : sig
      val request_path_template : unit -> string

      val post :
        body:Swagger_petstore.Definitions.Order.t ->
        ?ctx:Cohttp_lwt_unix.Client.ctx ->
        ?headers:Cohttp.Header.t ->
        Uri.t ->
        (Swagger_petstore.Definitions.Order.t, string) result Lwt.t
        [@@ocaml.doc
          {ocamlswagger|
@param body order placed for purchasing the pet|ocamlswagger}]
    end
  end

  module User : sig
    module Create_with_array : sig
      val request_path_template : unit -> string

      val post :
        body:Swagger_petstore.Definitions.User.t list ->
        ?ctx:Cohttp_lwt_unix.Client.ctx ->
        ?headers:Cohttp.Header.t ->
        Uri.t ->
        (unit, string) result Lwt.t
        [@@ocaml.doc
          {ocamlswagger|
@param body List of user object|ocamlswagger}]
    end

    module Create_with_list : sig
      val request_path_template : unit -> string

      val post :
        body:Swagger_petstore.Definitions.User.t list ->
        ?ctx:Cohttp_lwt_unix.Client.ctx ->
        ?headers:Cohttp.Header.t ->
        Uri.t ->
        (unit, string) result Lwt.t
        [@@ocaml.doc
          {ocamlswagger|
@param body List of user object|ocamlswagger}]
    end

    module Login : sig
      val request_path_template : unit -> string

      val get :
        username:string ->
        password:string ->
        ?ctx:Cohttp_lwt_unix.Client.ctx ->
        ?headers:Cohttp.Header.t ->
        Uri.t ->
        (string, string) result Lwt.t
        [@@ocaml.doc
          {ocamlswagger|
@param username The user name for login
@param password The password for login in clear text|ocamlswagger}]
    end

    module Logout : sig
      val request_path_template : unit -> string

      val get :
        ?ctx:Cohttp_lwt_unix.Client.ctx ->
        ?headers:Cohttp.Header.t ->
        Uri.t ->
        (unit, string) result Lwt.t
        [@@ocaml.doc {ocamlswagger||ocamlswagger}]
    end

    val request_path_template : unit -> string

    val post :
      body:Swagger_petstore.Definitions.User.t ->
      ?ctx:Cohttp_lwt_unix.Client.ctx ->
      ?headers:Cohttp.Header.t ->
      Uri.t ->
      (unit, string) result Lwt.t
      [@@ocaml.doc
        {ocamlswagger|This can only be done by the logged in user.
@param body Created user object|ocamlswagger}]
  end
end = struct
  module Definitions = struct
    module Api_response = struct
      type t = {
        code : (int32 option[@default None]);
        type_ : (string option[@default None]); [@key "type"]
        message : (string option[@default None]);
      }
      [@@deriving yojson]

      let make ?message ?type_ ?code () = { code; type_; message }
      let code t = t.code

      let set_code code t =
        { t with code } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let type_ t = t.type_

      let set_type_ type_ t =
        { t with type_ } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let message t = t.message

      let set_message message t =
        { t with message } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      module Object = Object.Make (struct
        type value = t [@@deriving yojson]
      end)
    end

    module Category = struct
      type t = {
        id : (int64 option[@default None]);
        name : (string option[@default None]);
      }
      [@@deriving yojson]

      let make ?name ?id () = { id; name }
      let id t = t.id

      let set_id id t =
        { t with id } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let name t = t.name

      let set_name name t =
        { t with name } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      module Object = Object.Make (struct
        type value = t [@@deriving yojson]
      end)
    end

    module Order = struct
      type t = {
        id : (int64 option[@default None]);
        pet_id : (int64 option[@default None]); [@key "petId"]
        quantity : (int32 option[@default None]);
        ship_date : (float option[@default None]); [@key "shipDate"]
        status : (string option[@default None]);
        complete : (bool option[@default None]);
      }
      [@@deriving yojson]

      let make ?complete ?status ?ship_date ?quantity ?pet_id ?id () =
        { id; pet_id; quantity; ship_date; status; complete }

      let id t = t.id

      let set_id id t =
        { t with id } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let pet_id t = t.pet_id

      let set_pet_id pet_id t =
        { t with pet_id } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let quantity t = t.quantity

      let set_quantity quantity t =
        { t with quantity } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let ship_date t = t.ship_date

      let set_ship_date ship_date t =
        { t with ship_date } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let status t = t.status

      let set_status status t =
        { t with status } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let complete t = t.complete

      let set_complete complete t =
        { t with complete } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      module Object = Object.Make (struct
        type value = t [@@deriving yojson]
      end)
    end

    module Pet = struct
      type t = {
        id : (int64 option[@default None]);
        category :
          (Swagger_petstore.Definitions.Category.t option[@default None]);
        name : string;
        photo_urls : string list; [@key "photoUrls"]
        tags : (Swagger_petstore.Definitions.Tag.t list option[@default None]);
        status : (string option[@default None]);
      }
      [@@deriving yojson]

      let make ?status ?tags ~photo_urls ~name ?category ?id () =
        { id; category; name; photo_urls; tags; status }

      let id t = t.id

      let set_id id t =
        { t with id } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let category t = t.category

      let set_category category t =
        { t with category } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let name t = t.name

      let set_name name t =
        { t with name } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let photo_urls t = t.photo_urls

      let set_photo_urls photo_urls t =
        { t with photo_urls } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let tags t = t.tags

      let set_tags tags t =
        { t with tags } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let status t = t.status

      let set_status status t =
        { t with status } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      module Object = Object.Make (struct
        type value = t [@@deriving yojson]
      end)
    end

    module Tag = struct
      type t = {
        id : (int64 option[@default None]);
        name : (string option[@default None]);
      }
      [@@deriving yojson]

      let make ?name ?id () = { id; name }
      let id t = t.id

      let set_id id t =
        { t with id } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let name t = t.name

      let set_name name t =
        { t with name } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      module Object = Object.Make (struct
        type value = t [@@deriving yojson]
      end)
    end

    module User = struct
      type t = {
        id : (int64 option[@default None]);
        username : (string option[@default None]);
        first_name : (string option[@default None]); [@key "firstName"]
        last_name : (string option[@default None]); [@key "lastName"]
        email : (string option[@default None]);
        password : (string option[@default None]);
        phone : (string option[@default None]);
        user_status : (int32 option[@default None]); [@key "userStatus"]
      }
      [@@deriving yojson]

      let make ?user_status ?phone ?password ?email ?last_name ?first_name
          ?username ?id () =
        {
          id;
          username;
          first_name;
          last_name;
          email;
          password;
          phone;
          user_status;
        }

      let id t = t.id

      let set_id id t =
        { t with id } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let username t = t.username

      let set_username username t =
        { t with username } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let first_name t = t.first_name

      let set_first_name first_name t =
        { t with first_name } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let last_name t = t.last_name

      let set_last_name last_name t =
        { t with last_name } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let email t = t.email

      let set_email email t =
        { t with email } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let password t = t.password

      let set_password password t =
        { t with password } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let phone t = t.phone

      let set_phone phone t =
        { t with phone } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      let user_status t = t.user_status

      let set_user_status user_status t =
        { t with user_status } [@ocaml.warning {ocamlswagger|-23|ocamlswagger}]

      module Object = Object.Make (struct
        type value = t [@@deriving yojson]
      end)
    end
  end

  module Pet = struct
    module Find_by_status = struct
      let request_path_template () = "/pet/findByStatus"

      let get ~status ?ctx ?headers uri =
        let query = [ ("status", String.concat "," (Array.to_list status)) ] in
        let path =
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (Printf.sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        let open Lwt.Infix in
        Cohttp_lwt_unix.Client.get ?ctx ?headers uri >>= fun (resp, body) ->
        let code =
          resp |> Cohttp_lwt_unix.Response.status |> Cohttp.Code.code_of_status
        in
        Cohttp_lwt.Body.to_string body >>= fun body ->
        ignore body;
        Lwt.return
          (if code >= 200 && code < 300 then
           Ok
             (let open Yojson.Safe in
             Util.convert_each Swagger_petstore.Definitions.Pet.t_of_yojson
               (from_string body))
          else Error (string_of_int code))
    end

    module Find_by_tags = struct
      let request_path_template () = "/pet/findByTags"

      let get ~tags ?ctx ?headers uri =
        let query = [ ("tags", String.concat "," (Array.to_list tags)) ] in
        let path =
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (Printf.sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        let open Lwt.Infix in
        Cohttp_lwt_unix.Client.get ?ctx ?headers uri >>= fun (resp, body) ->
        let code =
          resp |> Cohttp_lwt_unix.Response.status |> Cohttp.Code.code_of_status
        in
        Cohttp_lwt.Body.to_string body >>= fun body ->
        ignore body;
        Lwt.return
          (if code >= 200 && code < 300 then
           Ok
             (let open Yojson.Safe in
             Util.convert_each Swagger_petstore.Definitions.Pet.t_of_yojson
               (from_string body))
          else Error (string_of_int code))
    end

    let request_path_template () = "/pet"

    let put ~body ?ctx ?headers uri =
      let query = [] in
      let path =
        let path_params = [] in
        List.fold_left
          (fun path (name, value) ->
            let re = Re.Pcre.regexp (Printf.sprintf "\\{%s\\}" name) in
            Re.replace_string re ~by:value path)
          (request_path_template ()) path_params
      in
      let full_path = Uri.path uri ^ path in
      let uri = Uri.with_path uri full_path in
      let uri =
        Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
      in
      let headers = headers in
      let open Lwt.Infix in
      Cohttp_lwt_unix.Client.put ?ctx ?headers
        ?body:
          (Some
             (Cohttp_lwt.Body.of_string
                (Yojson.Safe.to_string
                   (Swagger_petstore.Definitions.Pet.yojson_of_t body))))
        uri
      >>= fun (resp, body) ->
      let code =
        resp |> Cohttp_lwt_unix.Response.status |> Cohttp.Code.code_of_status
      in
      Cohttp_lwt.Body.to_string body >>= fun body ->
      ignore body;
      Lwt.return
        (if code >= 200 && code < 300 then Ok ()
        else Error (string_of_int code))

    let post ~body ?ctx ?headers uri =
      let query = [] in
      let path =
        let path_params = [] in
        List.fold_left
          (fun path (name, value) ->
            let re = Re.Pcre.regexp (Printf.sprintf "\\{%s\\}" name) in
            Re.replace_string re ~by:value path)
          (request_path_template ()) path_params
      in
      let full_path = Uri.path uri ^ path in
      let uri = Uri.with_path uri full_path in
      let uri =
        Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
      in
      let headers = headers in
      let open Lwt.Infix in
      Cohttp_lwt_unix.Client.post ?ctx ?headers
        ?body:
          (Some
             (Cohttp_lwt.Body.of_string
                (Yojson.Safe.to_string
                   (Swagger_petstore.Definitions.Pet.yojson_of_t body))))
        uri
      >>= fun (resp, body) ->
      let code =
        resp |> Cohttp_lwt_unix.Response.status |> Cohttp.Code.code_of_status
      in
      Cohttp_lwt.Body.to_string body >>= fun body ->
      ignore body;
      Lwt.return
        (if code >= 200 && code < 300 then Ok ()
        else Error (string_of_int code))
  end

  module Store = struct
    module Inventory = struct
      let request_path_template () = "/store/inventory"

      let get ?ctx ?headers uri =
        let query = [] in
        let path =
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (Printf.sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        let open Lwt.Infix in
        Cohttp_lwt_unix.Client.get ?ctx ?headers uri >>= fun (resp, body) ->
        let code =
          resp |> Cohttp_lwt_unix.Response.status |> Cohttp.Code.code_of_status
        in
        Cohttp_lwt.Body.to_string body >>= fun body ->
        ignore body;
        Lwt.return
          (if code >= 200 && code < 300 then
           Ok (Object.Of_ints32.t_of_yojson (Yojson.Safe.from_string body))
          else Error (string_of_int code))
    end

    module Order = struct
      let request_path_template () = "/store/order"

      let post ~body ?ctx ?headers uri =
        let query = [] in
        let path =
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (Printf.sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        let open Lwt.Infix in
        Cohttp_lwt_unix.Client.post ?ctx ?headers
          ?body:
            (Some
               (Cohttp_lwt.Body.of_string
                  (Yojson.Safe.to_string
                     (Swagger_petstore.Definitions.Order.yojson_of_t body))))
          uri
        >>= fun (resp, body) ->
        let code =
          resp |> Cohttp_lwt_unix.Response.status |> Cohttp.Code.code_of_status
        in
        Cohttp_lwt.Body.to_string body >>= fun body ->
        let json = Yojson.Safe.from_string body in
        Lwt.return
          (if code >= 200 && code < 300 then
           Ok (Swagger_petstore.Definitions.Order.t_of_yojson json)
          else Error body)
    end
  end

  module User = struct
    module Create_with_array = struct
      let request_path_template () = "/user/createWithArray"

      let post ~body ?ctx ?headers uri =
        let query = [] in
        let path =
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (Printf.sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        let open Lwt.Infix in
        Cohttp_lwt_unix.Client.post ?ctx ?headers
          ?body:
            (Some
               (Cohttp_lwt.Body.of_string
                  (Yojson.Safe.to_string
                     (`List
                       (List.map Swagger_petstore.Definitions.User.yojson_of_t
                          body)))))
          uri
        >>= fun (resp, body) ->
        let code =
          resp |> Cohttp_lwt_unix.Response.status |> Cohttp.Code.code_of_status
        in
        Cohttp_lwt.Body.to_string body >>= fun body ->
        ignore body;
        Lwt.return
          (if code >= 200 && code < 300 then Ok ()
          else Error (string_of_int code))
    end

    module Create_with_list = struct
      let request_path_template () = "/user/createWithList"

      let post ~body ?ctx ?headers uri =
        let query = [] in
        let path =
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (Printf.sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        let open Lwt.Infix in
        Cohttp_lwt_unix.Client.post ?ctx ?headers
          ?body:
            (Some
               (Cohttp_lwt.Body.of_string
                  (Yojson.Safe.to_string
                     (`List
                       (List.map Swagger_petstore.Definitions.User.yojson_of_t
                          body)))))
          uri
        >>= fun (resp, body) ->
        let code =
          resp |> Cohttp_lwt_unix.Response.status |> Cohttp.Code.code_of_status
        in
        Cohttp_lwt.Body.to_string body >>= fun body ->
        ignore body;
        Lwt.return
          (if code >= 200 && code < 300 then Ok ()
          else Error (string_of_int code))
    end

    module Login = struct
      let request_path_template () = "/user/login"

      let get ~username ~password ?ctx ?headers uri =
        let query = [ ("username", username); ("password", password) ] in
        let path =
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (Printf.sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        let open Lwt.Infix in
        Cohttp_lwt_unix.Client.get ?ctx ?headers uri >>= fun (resp, body) ->
        let code =
          resp |> Cohttp_lwt_unix.Response.status |> Cohttp.Code.code_of_status
        in
        Cohttp_lwt.Body.to_string body >>= fun body ->
        ignore body;
        Lwt.return
          (if code >= 200 && code < 300 then Ok body
          else Error (string_of_int code))
    end

    module Logout = struct
      let request_path_template () = "/user/logout"

      let get ?ctx ?headers uri =
        let query = [] in
        let path =
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (Printf.sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        let open Lwt.Infix in
        Cohttp_lwt_unix.Client.get ?ctx ?headers uri >>= fun (resp, body) ->
        let code =
          resp |> Cohttp_lwt_unix.Response.status |> Cohttp.Code.code_of_status
        in
        Cohttp_lwt.Body.to_string body >>= fun body ->
        ignore body;
        Lwt.return
          (if code >= 200 && code < 300 then Ok ()
          else Error (string_of_int code))
    end

    let request_path_template () = "/user"

    let post ~body ?ctx ?headers uri =
      let query = [] in
      let path =
        let path_params = [] in
        List.fold_left
          (fun path (name, value) ->
            let re = Re.Pcre.regexp (Printf.sprintf "\\{%s\\}" name) in
            Re.replace_string re ~by:value path)
          (request_path_template ()) path_params
      in
      let full_path = Uri.path uri ^ path in
      let uri = Uri.with_path uri full_path in
      let uri =
        Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
      in
      let headers = headers in
      let open Lwt.Infix in
      Cohttp_lwt_unix.Client.post ?ctx ?headers
        ?body:
          (Some
             (Cohttp_lwt.Body.of_string
                (Yojson.Safe.to_string
                   (Swagger_petstore.Definitions.User.yojson_of_t body))))
        uri
      >>= fun (resp, body) ->
      let code =
        resp |> Cohttp_lwt_unix.Response.status |> Cohttp.Code.code_of_status
      in
      Cohttp_lwt.Body.to_string body >>= fun body ->
      ignore body;
      Lwt.return
        (if code >= 200 && code < 300 then Ok ()
        else Error (string_of_int code))
  end
end
