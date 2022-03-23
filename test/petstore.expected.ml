module Object = struct
  module type Value = sig
    type value

    val value_of_yojson : Yojson.Safe.t -> (value, string) result
    val value_to_yojson : value -> Yojson.Safe.t
  end

  module type S = sig
    type value
    type t = (string * value) list [@@deriving yojson]
  end

  module Make (V : Value) : S with type value := V.value = struct
    type t = (string * V.value) list [@@deriving yojson]

    let to_yojson obj =
      `Assoc (List.map (fun (k, v) -> (k, V.value_to_yojson v)) obj)

    let of_yojson (obj : Yojson.Safe.t) : (t, string) result =
      let rec loop acc = function
        | [] -> Ok (List.rev acc)
        | (k, v) :: obj -> (
            match V.value_of_yojson v with
            | Ok v -> loop ((k, v) :: acc) obj
            | Error e -> Error ("invalid object:" ^ e))
      in
      match obj with `Assoc obj -> loop [] obj | _ -> Error "invalid object"
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

  module Of_bools = Make (struct
    type value = bool [@@deriving yojson]
  end)
end

module rec Swagger_petstore : sig
  module Definitions : sig
    module Api_response : sig
      type t [@@deriving yojson]

      val make : ?message:string -> ?type_:string -> ?code:int -> unit -> t
      val code : t -> int option

      val set_code : int option -> t -> t
      (** Set the value of the code field. *)

      val type_ : t -> string option

      val set_type_ : string option -> t -> t
      (** Set the value of the type_ field. *)

      val message : t -> string option

      val set_message : string option -> t -> t
      (** Set the value of the message field. *)

      module Object : Object.S with type value := t
    end

    module Category : sig
      type t [@@deriving yojson]

      val make : ?name:string -> ?id:int -> unit -> t
      val id : t -> int option

      val set_id : int option -> t -> t
      (** Set the value of the id field. *)

      val name : t -> string option

      val set_name : string option -> t -> t
      (** Set the value of the name field. *)

      module Object : Object.S with type value := t
    end

    module Order : sig
      type t [@@deriving yojson]

      val make :
        ?complete:bool ->
        ?status:string ->
        ?ship_date:string ->
        ?quantity:int ->
        ?pet_id:int ->
        ?id:int ->
        unit ->
        t

      val id : t -> int option

      val set_id : int option -> t -> t
      (** Set the value of the id field. *)

      val pet_id : t -> int option

      val set_pet_id : int option -> t -> t
      (** Set the value of the pet_id field. *)

      val quantity : t -> int option

      val set_quantity : int option -> t -> t
      (** Set the value of the quantity field. *)

      val ship_date : t -> string option

      val set_ship_date : string option -> t -> t
      (** Set the value of the ship_date field. *)

      val status : t -> string option
      (** Order Status *)

      val set_status : string option -> t -> t
      (** Set the value of the status field. *)

      val complete : t -> bool option

      val set_complete : bool option -> t -> t
      (** Set the value of the complete field. *)

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
        ?id:int ->
        unit ->
        t

      val id : t -> int option

      val set_id : int option -> t -> t
      (** Set the value of the id field. *)

      val category : t -> Swagger_petstore.Definitions.Category.t option

      val set_category :
        Swagger_petstore.Definitions.Category.t option -> t -> t
      (** Set the value of the category field. *)

      val name : t -> string

      val set_name : string -> t -> t
      (** Set the value of the name field. *)

      val photo_urls : t -> string list

      val set_photo_urls : string list -> t -> t
      (** Set the value of the photo_urls field. *)

      val tags : t -> Swagger_petstore.Definitions.Tag.t list option

      val set_tags : Swagger_petstore.Definitions.Tag.t list option -> t -> t
      (** Set the value of the tags field. *)

      val status : t -> string option
      (** pet status in the store *)

      val set_status : string option -> t -> t
      (** Set the value of the status field. *)

      module Object : Object.S with type value := t
    end

    module Tag : sig
      type t [@@deriving yojson]

      val make : ?name:string -> ?id:int -> unit -> t
      val id : t -> int option

      val set_id : int option -> t -> t
      (** Set the value of the id field. *)

      val name : t -> string option

      val set_name : string option -> t -> t
      (** Set the value of the name field. *)

      module Object : Object.S with type value := t
    end

    module User : sig
      type t [@@deriving yojson]

      val make :
        ?user_status:int ->
        ?phone:string ->
        ?password:string ->
        ?email:string ->
        ?last_name:string ->
        ?first_name:string ->
        ?username:string ->
        ?id:int ->
        unit ->
        t

      val id : t -> int option

      val set_id : int option -> t -> t
      (** Set the value of the id field. *)

      val username : t -> string option

      val set_username : string option -> t -> t
      (** Set the value of the username field. *)

      val first_name : t -> string option

      val set_first_name : string option -> t -> t
      (** Set the value of the first_name field. *)

      val last_name : t -> string option

      val set_last_name : string option -> t -> t
      (** Set the value of the last_name field. *)

      val email : t -> string option

      val set_email : string option -> t -> t
      (** Set the value of the email field. *)

      val password : t -> string option

      val set_password : string option -> t -> t
      (** Set the value of the password field. *)

      val phone : t -> string option

      val set_phone : string option -> t -> t
      (** Set the value of the phone field. *)

      val user_status : t -> int option
      (** User Status *)

      val set_user_status : int option -> t -> t
      (** Set the value of the user_status field. *)

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
      (** Multiple status values can be provided with comma separated strings

          @param status Status values that need to be considered for filter *)
    end

    module Find_by_tags : sig
      val request_path_template : unit -> string

      val get :
        tags:string array ->
        ?ctx:Cohttp_lwt_unix.Client.ctx ->
        ?headers:Cohttp.Header.t ->
        Uri.t ->
        (Swagger_petstore.Definitions.Pet.t list, string) result Lwt.t
      (** Multiple tags can be provided with comma separated strings. Use tag1, tag2, tag3 for testing.

          @param tags Tags to filter by *)
    end

    val request_path_template : unit -> string

    val put :
      body:Swagger_petstore.Definitions.Pet.t ->
      ?ctx:Cohttp_lwt_unix.Client.ctx ->
      ?headers:Cohttp.Header.t ->
      Uri.t ->
      (unit, string) result Lwt.t
    (** 

        @param body Pet object that needs to be added to the store *)

    val post :
      body:Swagger_petstore.Definitions.Pet.t ->
      ?ctx:Cohttp_lwt_unix.Client.ctx ->
      ?headers:Cohttp.Header.t ->
      Uri.t ->
      (unit, string) result Lwt.t
    (** 

        @param body Pet object that needs to be added to the store *)
  end

  module Store : sig
    module Inventory : sig
      val request_path_template : unit -> string

      val get :
        ?ctx:Cohttp_lwt_unix.Client.ctx ->
        ?headers:Cohttp.Header.t ->
        Uri.t ->
        (Object.Of_ints.t, string) result Lwt.t
      (** Returns a map of status codes to quantities *)
    end

    module Order : sig
      val request_path_template : unit -> string

      val post :
        body:Swagger_petstore.Definitions.Order.t ->
        ?ctx:Cohttp_lwt_unix.Client.ctx ->
        ?headers:Cohttp.Header.t ->
        Uri.t ->
        (Swagger_petstore.Definitions.Order.t, string) result Lwt.t
      (** 

          @param body order placed for purchasing the pet *)
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
      (** 

          @param body List of user object *)
    end

    module Create_with_list : sig
      val request_path_template : unit -> string

      val post :
        body:Swagger_petstore.Definitions.User.t list ->
        ?ctx:Cohttp_lwt_unix.Client.ctx ->
        ?headers:Cohttp.Header.t ->
        Uri.t ->
        (unit, string) result Lwt.t
      (** 

          @param body List of user object *)
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
      (** 

          @param username The user name for login

          @param password The password for login in clear text *)
    end

    module Logout : sig
      val request_path_template : unit -> string

      val get :
        ?ctx:Cohttp_lwt_unix.Client.ctx ->
        ?headers:Cohttp.Header.t ->
        Uri.t ->
        (unit, string) result Lwt.t
      (**  *)
    end

    val request_path_template : unit -> string

    val post :
      body:Swagger_petstore.Definitions.User.t ->
      ?ctx:Cohttp_lwt_unix.Client.ctx ->
      ?headers:Cohttp.Header.t ->
      Uri.t ->
      (unit, string) result Lwt.t
    (** This can only be done by the logged in user.

        @param body Created user object *)
  end
end = struct
  module Definitions = struct
    module Api_response = struct
      type t = {
        message : (string option[@default None]);
        type_ : (string option[@default None]); [@key "type"]
        code : (int option[@default None]);
      }
      [@@deriving yojson]

      let make ?message ?type_ ?code () = { message; type_; code }
      let code t = t.code
      let set_code code t = { t with code }
      let type_ t = t.type_
      let set_type_ type_ t = { t with type_ }
      let message t = t.message
      let set_message message t = { t with message }

      module Object = Object.Make (struct
        type value = t [@@deriving yojson]
      end)
    end

    module Category = struct
      type t = {
        name : (string option[@default None]);
        id : (int option[@default None]);
      }
      [@@deriving yojson]

      let make ?name ?id () = { name; id }
      let id t = t.id
      let set_id id t = { t with id }
      let name t = t.name
      let set_name name t = { t with name }

      module Object = Object.Make (struct
        type value = t [@@deriving yojson]
      end)
    end

    module Order = struct
      type t = {
        complete : (bool option[@default None]);
        status : (string option[@default None]);
        ship_date : (string option[@default None]); [@key "shipDate"]
        quantity : (int option[@default None]);
        pet_id : (int option[@default None]); [@key "petId"]
        id : (int option[@default None]);
      }
      [@@deriving yojson]

      let make ?complete ?status ?ship_date ?quantity ?pet_id ?id () =
        { complete; status; ship_date; quantity; pet_id; id }

      let id t = t.id
      let set_id id t = { t with id }
      let pet_id t = t.pet_id
      let set_pet_id pet_id t = { t with pet_id }
      let quantity t = t.quantity
      let set_quantity quantity t = { t with quantity }
      let ship_date t = t.ship_date
      let set_ship_date ship_date t = { t with ship_date }
      let status t = t.status
      let set_status status t = { t with status }
      let complete t = t.complete
      let set_complete complete t = { t with complete }

      module Object = Object.Make (struct
        type value = t [@@deriving yojson]
      end)
    end

    module Pet = struct
      type t = {
        status : (string option[@default None]);
        tags : (Swagger_petstore.Definitions.Tag.t list option[@default None]);
        photo_urls : string list; [@key "photoUrls"]
        name : string;
        category :
          (Swagger_petstore.Definitions.Category.t option[@default None]);
        id : (int option[@default None]);
      }
      [@@deriving yojson]

      let make ?status ?tags ~photo_urls ~name ?category ?id () =
        { status; tags; photo_urls; name; category; id }

      let id t = t.id
      let set_id id t = { t with id }
      let category t = t.category
      let set_category category t = { t with category }
      let name t = t.name
      let set_name name t = { t with name }
      let photo_urls t = t.photo_urls
      let set_photo_urls photo_urls t = { t with photo_urls }
      let tags t = t.tags
      let set_tags tags t = { t with tags }
      let status t = t.status
      let set_status status t = { t with status }

      module Object = Object.Make (struct
        type value = t [@@deriving yojson]
      end)
    end

    module Tag = struct
      type t = {
        name : (string option[@default None]);
        id : (int option[@default None]);
      }
      [@@deriving yojson]

      let make ?name ?id () = { name; id }
      let id t = t.id
      let set_id id t = { t with id }
      let name t = t.name
      let set_name name t = { t with name }

      module Object = Object.Make (struct
        type value = t [@@deriving yojson]
      end)
    end

    module User = struct
      type t = {
        user_status : (int option[@default None]); [@key "userStatus"]
        phone : (string option[@default None]);
        password : (string option[@default None]);
        email : (string option[@default None]);
        last_name : (string option[@default None]); [@key "lastName"]
        first_name : (string option[@default None]); [@key "firstName"]
        username : (string option[@default None]);
        id : (int option[@default None]);
      }
      [@@deriving yojson]

      let make ?user_status ?phone ?password ?email ?last_name ?first_name
          ?username ?id () =
        {
          user_status;
          phone;
          password;
          email;
          last_name;
          first_name;
          username;
          id;
        }

      let id t = t.id
      let set_id id t = { t with id }
      let username t = t.username
      let set_username username t = { t with username }
      let first_name t = t.first_name
      let set_first_name first_name t = { t with first_name }
      let last_name t = t.last_name
      let set_last_name last_name t = { t with last_name }
      let email t = t.email
      let set_email email t = { t with email }
      let password t = t.password
      let set_password password t = { t with password }
      let phone t = t.phone
      let set_phone phone t = { t with phone }
      let user_status t = t.user_status
      let set_user_status user_status t = { t with user_status }

      module Object = Object.Make (struct
        type value = t [@@deriving yojson]
      end)
    end
  end

  module Pet = struct
    module Find_by_status = struct
      let request_path_template () = "/pet/findByStatus"

      let get ~status ?ctx ?headers uri =
        let open Lwt.Infix in
        let open Cohttp in
        let open Cohttp_lwt_unix in
        let module Body = Cohttp_lwt.Body in
        let query = [ ("status", string_of_string array status) ] in
        let path =
          let open Printf in
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        Client.get ?ctx ?headers uri >>= fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        Body.to_string body >>= fun body ->
        ignore body;
        Lwt.return
          (if code >= 200 && code < 300 then
           Ok (Swagger_petstore.Definitions.Pet.t list_of_string body)
          else Error (string_of_int code))
    end

    module Find_by_tags = struct
      let request_path_template () = "/pet/findByTags"

      let get ~tags ?ctx ?headers uri =
        let open Lwt.Infix in
        let open Cohttp in
        let open Cohttp_lwt_unix in
        let module Body = Cohttp_lwt.Body in
        let query = [ ("tags", string_of_string array tags) ] in
        let path =
          let open Printf in
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        Client.get ?ctx ?headers uri >>= fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        Body.to_string body >>= fun body ->
        ignore body;
        Lwt.return
          (if code >= 200 && code < 300 then
           Ok (Swagger_petstore.Definitions.Pet.t list_of_string body)
          else Error (string_of_int code))
    end

    let request_path_template () = "/pet"

    let put ~body ?ctx ?headers uri =
      let open Lwt.Infix in
      let open Cohttp in
      let open Cohttp_lwt_unix in
      let module Body = Cohttp_lwt.Body in
      let query = [] in
      let path =
        let open Printf in
        let path_params = [] in
        List.fold_left
          (fun path (name, value) ->
            let re = Re.Pcre.regexp (sprintf "\\{%s\\}" name) in
            Re.replace_string re ~by:value path)
          (request_path_template ()) path_params
      in
      let full_path = Uri.path uri ^ path in
      let uri = Uri.with_path uri full_path in
      let uri =
        Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
      in
      let headers = headers in
      Client.put ?ctx ?headers
        ?body:
          (Some
             (Body.of_string
                (Yojson.Safe.to_string
                   (Swagger_petstore.Definitions.Pet.to_yojson body))))
        uri
      >>= fun (resp, body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      Body.to_string body >>= fun body ->
      ignore body;
      Lwt.return
        (if code >= 200 && code < 300 then Ok ()
        else Error (string_of_int code))

    let post ~body ?ctx ?headers uri =
      let open Lwt.Infix in
      let open Cohttp in
      let open Cohttp_lwt_unix in
      let module Body = Cohttp_lwt.Body in
      let query = [] in
      let path =
        let open Printf in
        let path_params = [] in
        List.fold_left
          (fun path (name, value) ->
            let re = Re.Pcre.regexp (sprintf "\\{%s\\}" name) in
            Re.replace_string re ~by:value path)
          (request_path_template ()) path_params
      in
      let full_path = Uri.path uri ^ path in
      let uri = Uri.with_path uri full_path in
      let uri =
        Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
      in
      let headers = headers in
      Client.post ?ctx ?headers
        ?body:
          (Some
             (Body.of_string
                (Yojson.Safe.to_string
                   (Swagger_petstore.Definitions.Pet.to_yojson body))))
        uri
      >>= fun (resp, body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      Body.to_string body >>= fun body ->
      ignore body;
      Lwt.return
        (if code >= 200 && code < 300 then Ok ()
        else Error (string_of_int code))
  end

  module Store = struct
    module Inventory = struct
      let request_path_template () = "/store/inventory"

      let get ?ctx ?headers uri =
        let open Lwt.Infix in
        let open Cohttp in
        let open Cohttp_lwt_unix in
        let module Body = Cohttp_lwt.Body in
        let query = [] in
        let path =
          let open Printf in
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        Client.get ?ctx ?headers uri >>= fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        Body.to_string body >>= fun body ->
        ignore body;
        Lwt.return
          (if code >= 200 && code < 300 then
           Ok (Object.Of_ints.t_of_string body)
          else Error (string_of_int code))
    end

    module Order = struct
      let request_path_template () = "/store/order"

      let post ~body ?ctx ?headers uri =
        let open Lwt.Infix in
        let open Cohttp in
        let open Cohttp_lwt_unix in
        let module Body = Cohttp_lwt.Body in
        let query = [] in
        let path =
          let open Printf in
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        Client.post ?ctx ?headers
          ?body:
            (Some
               (Body.of_string
                  (Yojson.Safe.to_string
                     (Swagger_petstore.Definitions.Order.to_yojson body))))
          uri
        >>= fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        Body.to_string body >>= fun body ->
        let json = Yojson.Safe.from_string body in
        Lwt.return
          (if code >= 200 && code < 300 then
           Swagger_petstore.Definitions.Order.of_yojson json
          else Error body)
    end
  end

  module User = struct
    module Create_with_array = struct
      let request_path_template () = "/user/createWithArray"

      let post ~body ?ctx ?headers uri =
        let open Lwt.Infix in
        let open Cohttp in
        let open Cohttp_lwt_unix in
        let module Body = Cohttp_lwt.Body in
        let query = [] in
        let path =
          let open Printf in
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        Client.post ?ctx ?headers
          ?body:
            (Some
               (Body.of_string
                  (Yojson.Safe.to_string
                     (Swagger_petstore.Definitions.User.to_yojson body))))
          uri
        >>= fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        Body.to_string body >>= fun body ->
        ignore body;
        Lwt.return
          (if code >= 200 && code < 300 then Ok ()
          else Error (string_of_int code))
    end

    module Create_with_list = struct
      let request_path_template () = "/user/createWithList"

      let post ~body ?ctx ?headers uri =
        let open Lwt.Infix in
        let open Cohttp in
        let open Cohttp_lwt_unix in
        let module Body = Cohttp_lwt.Body in
        let query = [] in
        let path =
          let open Printf in
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        Client.post ?ctx ?headers
          ?body:
            (Some
               (Body.of_string
                  (Yojson.Safe.to_string
                     (Swagger_petstore.Definitions.User.to_yojson body))))
          uri
        >>= fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        Body.to_string body >>= fun body ->
        ignore body;
        Lwt.return
          (if code >= 200 && code < 300 then Ok ()
          else Error (string_of_int code))
    end

    module Login = struct
      let request_path_template () = "/user/login"

      let get ~username ~password ?ctx ?headers uri =
        let open Lwt.Infix in
        let open Cohttp in
        let open Cohttp_lwt_unix in
        let module Body = Cohttp_lwt.Body in
        let query = [ ("username", username); ("password", password) ] in
        let path =
          let open Printf in
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        Client.get ?ctx ?headers uri >>= fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        Body.to_string body >>= fun body ->
        ignore body;
        Lwt.return
          (if code >= 200 && code < 300 then Ok body
          else Error (string_of_int code))
    end

    module Logout = struct
      let request_path_template () = "/user/logout"

      let get ?ctx ?headers uri =
        let open Lwt.Infix in
        let open Cohttp in
        let open Cohttp_lwt_unix in
        let module Body = Cohttp_lwt.Body in
        let query = [] in
        let path =
          let open Printf in
          let path_params = [] in
          List.fold_left
            (fun path (name, value) ->
              let re = Re.Pcre.regexp (sprintf "\\{%s\\}" name) in
              Re.replace_string re ~by:value path)
            (request_path_template ()) path_params
        in
        let full_path = Uri.path uri ^ path in
        let uri = Uri.with_path uri full_path in
        let uri =
          Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
        in
        let headers = headers in
        Client.get ?ctx ?headers uri >>= fun (resp, body) ->
        let code = resp |> Response.status |> Code.code_of_status in
        Body.to_string body >>= fun body ->
        ignore body;
        Lwt.return
          (if code >= 200 && code < 300 then Ok ()
          else Error (string_of_int code))
    end

    let request_path_template () = "/user"

    let post ~body ?ctx ?headers uri =
      let open Lwt.Infix in
      let open Cohttp in
      let open Cohttp_lwt_unix in
      let module Body = Cohttp_lwt.Body in
      let query = [] in
      let path =
        let open Printf in
        let path_params = [] in
        List.fold_left
          (fun path (name, value) ->
            let re = Re.Pcre.regexp (sprintf "\\{%s\\}" name) in
            Re.replace_string re ~by:value path)
          (request_path_template ()) path_params
      in
      let full_path = Uri.path uri ^ path in
      let uri = Uri.with_path uri full_path in
      let uri =
        Uri.with_query' uri (List.filter (fun (_k, v) -> v <> "") query)
      in
      let headers = headers in
      Client.post ?ctx ?headers
        ?body:
          (Some
             (Body.of_string
                (Yojson.Safe.to_string
                   (Swagger_petstore.Definitions.User.to_yojson body))))
        uri
      >>= fun (resp, body) ->
      let code = resp |> Response.status |> Code.code_of_status in
      Body.to_string body >>= fun body ->
      ignore body;
      Lwt.return
        (if code >= 200 && code < 300 then Ok ()
        else Error (string_of_int code))
  end
end
