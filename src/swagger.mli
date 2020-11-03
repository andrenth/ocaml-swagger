val codegen :
  path_base:string ->
  definition_base:string ->
  reference_base:string ->
  reference_root:string -> ?output:out_channel -> input:string -> unit
(** Generates an OCaml module representation of the Swagger schema located at [input]. *)


(** The Swagger schema definitions. *)
module Schema : sig
  include module type of Swagger_t (** @inline *)

  val validate_collection_format :
    Atdgen_runtime.Util.Validation.path -> collection_format -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!collection_format}. *)

  val create_external_documentation :
    ?description: string ->
    url: string ->
    unit -> external_documentation
    (** Create a record of type {!external_documentation}. *)

  val validate_external_documentation :
    Atdgen_runtime.Util.Validation.path -> external_documentation -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!external_documentation}. *)

  val validate_items_kind :
    Atdgen_runtime.Util.Validation.path -> items_kind -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!items_kind}. *)

  val validate_json :
    Atdgen_runtime.Util.Validation.path -> json -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!json}. *)

  val validate_reference :
    Atdgen_runtime.Util.Validation.path -> reference -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!reference}. *)

  val validate_schema_kind :
    Atdgen_runtime.Util.Validation.path -> schema_kind -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!schema_kind}. *)

  val create_xml :
    ?name: string ->
    ?namespace: string ->
    ?prefix: string ->
    ?attribute: bool ->
    ?wrapped: bool ->
    unit -> xml
    (** Create a record of type {!xml}. *)

  val validate_xml :
    Atdgen_runtime.Util.Validation.path -> xml -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!xml}. *)

  val create_schema :
    ?ref: reference ->
    ?format: string ->
    ?title: string ->
    ?description: string ->
    ?default: json ->
    ?multiple_of: int ->
    ?maximum: int ->
    ?exclusive_maximum: bool ->
    ?minimum: int ->
    ?exclusive_minimum: bool ->
    ?max_length: int ->
    ?min_length: int ->
    ?pattern: string ->
    ?max_items: int ->
    ?min_items: int ->
    ?unique_items: bool ->
    ?max_properties: int ->
    ?min_properties: int ->
    ?required: string list ->
    ?enum: json list ->
    ?kind: schema_kind ->
    ?items: schema ->
    ?all_of: json ->
    ?properties: (string * schema) list ->
    ?additional_properties: schema ->
    ?discriminator: string ->
    ?read_only: bool ->
    ?xml: xml ->
    ?external_docs: external_documentation ->
    ?example: json ->
    unit -> schema
    (** Create a record of type {!schema}. *)

  val validate_schema :
    Atdgen_runtime.Util.Validation.path -> schema -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!schema}. *)

  val create_items :
    kind: items_kind ->
    ?format: string ->
    ?items: items ->
    ?collection_format: collection_format ->
    ?default: json ->
    ?maximum: int ->
    ?exclusive_maximum: bool ->
    ?minimum: int ->
    ?exclusive_minimum: bool ->
    ?max_length: int ->
    ?min_length: int ->
    ?pattern: string ->
    ?max_items: int ->
    ?min_items: int ->
    ?unique_items: bool ->
    ?enum: json list ->
    ?multiple_of: int ->
    unit -> items
    (** Create a record of type {!items}. *)

  val validate_items :
    Atdgen_runtime.Util.Validation.path -> items -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!items}. *)

  val create_tag :
    name: string ->
    ?description: string ->
    ?external_docs: external_documentation ->
    unit -> tag
    (** Create a record of type {!tag}. *)

  val validate_tag :
    Atdgen_runtime.Util.Validation.path -> tag -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!tag}. *)

  val validate_security_scheme_type :
    Atdgen_runtime.Util.Validation.path -> security_scheme_type -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!security_scheme_type}. *)

  val validate_security_scheme_location :
    Atdgen_runtime.Util.Validation.path -> security_scheme_location -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!security_scheme_location}. *)

  val validate_security_scheme_flow :
    Atdgen_runtime.Util.Validation.path -> security_scheme_flow -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!security_scheme_flow}. *)

  val validate_scopes :
    Atdgen_runtime.Util.Validation.path -> scopes -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!scopes}. *)

  val create_security_scheme :
    kind: security_scheme_type ->
    ?description: string ->
    name: string ->
    location: security_scheme_location ->
    ?flow: security_scheme_flow ->
    ?authorization_url: string ->
    ?token_url: string ->
    ?scopes: scopes ->
    unit -> security_scheme
    (** Create a record of type {!security_scheme}. *)

  val validate_security_scheme :
    Atdgen_runtime.Util.Validation.path -> security_scheme -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!security_scheme}. *)

  val validate_security_requirement :
    Atdgen_runtime.Util.Validation.path -> security_requirement -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!security_requirement}. *)

  val validate_security_definitions :
    Atdgen_runtime.Util.Validation.path -> security_definitions -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!security_definitions}. *)

  val validate_scheme :
    Atdgen_runtime.Util.Validation.path -> scheme -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!scheme}. *)

  val validate_kind :
    Atdgen_runtime.Util.Validation.path -> kind -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!kind}. *)

  val create_header :
    ?description: string ->
    kind: kind ->
    ?format: string ->
    ?items: items ->
    ?collection_format: collection_format ->
    ?default: json ->
    ?maximum: int ->
    ?exclusive_maximum: bool ->
    ?minimum: int ->
    ?exclusive_minimum: bool ->
    ?max_length: int ->
    ?min_length: int ->
    ?pattern: string ->
    ?max_items: int ->
    ?min_items: int ->
    ?unique_items: bool ->
    ?enum: json list ->
    ?multiple_of: int ->
    unit -> header
    (** Create a record of type {!header}. *)

  val validate_header :
    Atdgen_runtime.Util.Validation.path -> header -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!header}. *)

  val validate_headers :
    Atdgen_runtime.Util.Validation.path -> headers -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!headers}. *)

  val validate_example :
    Atdgen_runtime.Util.Validation.path -> example -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!example}. *)

  val create_response :
    description: string ->
    ?schema: schema ->
    ?headers: headers ->
    ?examples: example ->
    unit -> response
    (** Create a record of type {!response}. *)

  val validate_response :
    Atdgen_runtime.Util.Validation.path -> response -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!response}. *)

  val validate_responses_definitions :
    Atdgen_runtime.Util.Validation.path -> responses_definitions -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!responses_definitions}. *)

  val validate_responses :
    Atdgen_runtime.Util.Validation.path -> responses -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!responses}. *)

  val create_operation :
    ?tags: string list ->
    ?summary: string ->
    ?description: string ->
    ?external_docs: external_documentation ->
    ?operation_id: string ->
    ?consumes: string list ->
    ?produces: string list ->
    ?parameters: json list ->
    responses: responses ->
    ?schemes: scheme list ->
    ?deprecated: bool ->
    ?security: security_requirement list ->
    unit -> operation
    (** Create a record of type {!operation}. *)

  val validate_operation :
    Atdgen_runtime.Util.Validation.path -> operation -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!operation}. *)

  val create_path_item :
    ?ref: string ->
    ?get: operation ->
    ?put: operation ->
    ?post: operation ->
    ?delete: operation ->
    ?options: operation ->
    ?head: operation ->
    ?patch: operation ->
    ?parameters: json list ->
    unit -> path_item
    (** Create a record of type {!path_item}. *)

  val validate_path_item :
    Atdgen_runtime.Util.Validation.path -> path_item -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!path_item}. *)

  val validate_paths :
    Atdgen_runtime.Util.Validation.path -> paths -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!paths}. *)

  val validate_location :
    Atdgen_runtime.Util.Validation.path -> location -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!location}. *)

  val create_parameter :
    name: string ->
    location: location ->
    ?description: string ->
    ?required: bool ->
    ?schema: schema ->
    ?kind: kind ->
    ?format: string ->
    ?allow_empty_value: bool ->
    ?items: items ->
    ?collection_format: collection_format ->
    ?default: json ->
    ?maximum: int ->
    ?exclusive_maximum: bool ->
    ?minimum: int ->
    ?exclusive_minimum: bool ->
    ?max_length: int ->
    ?min_length: int ->
    ?pattern: string ->
    ?max_items: int ->
    ?min_items: int ->
    ?unique_items: bool ->
    ?enum: json list ->
    ?multiple_of: int ->
    unit -> parameter
    (** Create a record of type {!parameter}. *)

  val validate_parameter :
    Atdgen_runtime.Util.Validation.path -> parameter -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!parameter}. *)

  val validate_parameters_definitions :
    Atdgen_runtime.Util.Validation.path -> parameters_definitions -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!parameters_definitions}. *)

  val create_license :
    name: string ->
    ?url: string ->
    unit -> license
    (** Create a record of type {!license}. *)

  val validate_license :
    Atdgen_runtime.Util.Validation.path -> license -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!license}. *)

  val create_contact :
    ?name: string ->
    ?url: string ->
    ?email: string ->
    unit -> contact
    (** Create a record of type {!contact}. *)

  val validate_contact :
    Atdgen_runtime.Util.Validation.path -> contact -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!contact}. *)

  val create_info :
    title: string ->
    ?description: string ->
    ?terms_of_service: string ->
    ?contact: contact ->
    ?license: license ->
    version: string ->
    unit -> info
    (** Create a record of type {!info}. *)

  val validate_info :
    Atdgen_runtime.Util.Validation.path -> info -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!info}. *)

  val validate_definitions :
    Atdgen_runtime.Util.Validation.path -> definitions -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!definitions}. *)

  val create_swagger :
    swagger: string ->
    info: info ->
    ?host: string ->
    ?base_path: string ->
    ?schemes: scheme list ->
    ?consumes: string list ->
    ?produces: string list ->
    paths: paths ->
    ?definitions: definitions ->
    ?parameters: parameters_definitions ->
    ?responses: responses_definitions ->
    ?security_definitions: security_definitions ->
    ?security: security_requirement list ->
    ?tags: tag list ->
    ?external_docs: external_documentation ->
    unit -> swagger
    (** Create a record of type {!swagger}. *)

  val validate_swagger :
    Atdgen_runtime.Util.Validation.path -> swagger -> Atdgen_runtime.Util.Validation.error option
    (** Validate a value of type {!swagger}. *)
end

(** JSON conversion functions for swagger schema. *)
module Json : sig
  open Schema

  val write_collection_format :
    Bi_outbuf.t -> collection_format -> unit
    (** Output a JSON value of type {!collection_format}. *)

  val string_of_collection_format :
    ?len:int -> collection_format -> string
    (** Serialize a value of type {!collection_format}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_collection_format :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> collection_format
    (** Input JSON data of type {!collection_format}. *)

  val collection_format_of_string :
    string -> collection_format
    (** Deserialize JSON data of type {!collection_format}. *)

  val write_external_documentation :
    Bi_outbuf.t -> external_documentation -> unit
    (** Output a JSON value of type {!external_documentation}. *)

  val string_of_external_documentation :
    ?len:int -> external_documentation -> string
    (** Serialize a value of type {!external_documentation}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_external_documentation :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> external_documentation
    (** Input JSON data of type {!external_documentation}. *)

  val external_documentation_of_string :
    string -> external_documentation
    (** Deserialize JSON data of type {!external_documentation}. *)

  val write_items_kind :
    Bi_outbuf.t -> items_kind -> unit
    (** Output a JSON value of type {!items_kind}. *)

  val string_of_items_kind :
    ?len:int -> items_kind -> string
    (** Serialize a value of type {!items_kind}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_items_kind :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> items_kind
    (** Input JSON data of type {!items_kind}. *)

  val items_kind_of_string :
    string -> items_kind
    (** Deserialize JSON data of type {!items_kind}. *)

  val write_json :
    Bi_outbuf.t -> json -> unit
    (** Output a JSON value of type {!json}. *)

  val string_of_json :
    ?len:int -> json -> string
    (** Serialize a value of type {!json}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_json :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> json
    (** Input JSON data of type {!json}. *)

  val json_of_string :
    string -> json
    (** Deserialize JSON data of type {!json}. *)

  val write_reference :
    Bi_outbuf.t -> reference -> unit
    (** Output a JSON value of type {!reference}. *)

  val string_of_reference :
    ?len:int -> reference -> string
    (** Serialize a value of type {!reference}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_reference :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> reference
    (** Input JSON data of type {!reference}. *)

  val reference_of_string :
    string -> reference
    (** Deserialize JSON data of type {!reference}. *)

  val write_schema_kind :
    Bi_outbuf.t -> schema_kind -> unit
    (** Output a JSON value of type {!schema_kind}. *)

  val string_of_schema_kind :
    ?len:int -> schema_kind -> string
    (** Serialize a value of type {!schema_kind}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_schema_kind :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> schema_kind
    (** Input JSON data of type {!schema_kind}. *)

  val schema_kind_of_string :
    string -> schema_kind
    (** Deserialize JSON data of type {!schema_kind}. *)

  val write_xml :
    Bi_outbuf.t -> xml -> unit
    (** Output a JSON value of type {!xml}. *)

  val string_of_xml :
    ?len:int -> xml -> string
    (** Serialize a value of type {!xml}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_xml :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> xml
    (** Input JSON data of type {!xml}. *)

  val xml_of_string :
    string -> xml
    (** Deserialize JSON data of type {!xml}. *)

  val write_schema :
    Bi_outbuf.t -> schema -> unit
    (** Output a JSON value of type {!schema}. *)

  val string_of_schema :
    ?len:int -> schema -> string
    (** Serialize a value of type {!schema}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_schema :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> schema
    (** Input JSON data of type {!schema}. *)

  val schema_of_string :
    string -> schema
    (** Deserialize JSON data of type {!schema}. *)

  val write_items :
    Bi_outbuf.t -> items -> unit
    (** Output a JSON value of type {!items}. *)

  val string_of_items :
    ?len:int -> items -> string
    (** Serialize a value of type {!items}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_items :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> items
    (** Input JSON data of type {!items}. *)

  val items_of_string :
    string -> items
    (** Deserialize JSON data of type {!items}. *)

  val write_tag :
    Bi_outbuf.t -> tag -> unit
    (** Output a JSON value of type {!tag}. *)

  val string_of_tag :
    ?len:int -> tag -> string
    (** Serialize a value of type {!tag}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_tag :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> tag
    (** Input JSON data of type {!tag}. *)

  val tag_of_string :
    string -> tag
    (** Deserialize JSON data of type {!tag}. *)

  val write_security_scheme_type :
    Bi_outbuf.t -> security_scheme_type -> unit
    (** Output a JSON value of type {!security_scheme_type}. *)

  val string_of_security_scheme_type :
    ?len:int -> security_scheme_type -> string
    (** Serialize a value of type {!security_scheme_type}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_security_scheme_type :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> security_scheme_type
    (** Input JSON data of type {!security_scheme_type}. *)

  val security_scheme_type_of_string :
    string -> security_scheme_type
    (** Deserialize JSON data of type {!security_scheme_type}. *)

  val write_security_scheme_location :
    Bi_outbuf.t -> security_scheme_location -> unit
    (** Output a JSON value of type {!security_scheme_location}. *)

  val string_of_security_scheme_location :
    ?len:int -> security_scheme_location -> string
    (** Serialize a value of type {!security_scheme_location}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_security_scheme_location :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> security_scheme_location
    (** Input JSON data of type {!security_scheme_location}. *)

  val security_scheme_location_of_string :
    string -> security_scheme_location
    (** Deserialize JSON data of type {!security_scheme_location}. *)

  val write_security_scheme_flow :
    Bi_outbuf.t -> security_scheme_flow -> unit
    (** Output a JSON value of type {!security_scheme_flow}. *)

  val string_of_security_scheme_flow :
    ?len:int -> security_scheme_flow -> string
    (** Serialize a value of type {!security_scheme_flow}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_security_scheme_flow :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> security_scheme_flow
    (** Input JSON data of type {!security_scheme_flow}. *)

  val security_scheme_flow_of_string :
    string -> security_scheme_flow
    (** Deserialize JSON data of type {!security_scheme_flow}. *)

  val write_scopes :
    Bi_outbuf.t -> scopes -> unit
    (** Output a JSON value of type {!scopes}. *)

  val string_of_scopes :
    ?len:int -> scopes -> string
    (** Serialize a value of type {!scopes}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_scopes :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> scopes
    (** Input JSON data of type {!scopes}. *)

  val scopes_of_string :
    string -> scopes
    (** Deserialize JSON data of type {!scopes}. *)

  val write_security_scheme :
    Bi_outbuf.t -> security_scheme -> unit
    (** Output a JSON value of type {!security_scheme}. *)

  val string_of_security_scheme :
    ?len:int -> security_scheme -> string
    (** Serialize a value of type {!security_scheme}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_security_scheme :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> security_scheme
    (** Input JSON data of type {!security_scheme}. *)

  val security_scheme_of_string :
    string -> security_scheme
    (** Deserialize JSON data of type {!security_scheme}. *)

  val write_security_requirement :
    Bi_outbuf.t -> security_requirement -> unit
    (** Output a JSON value of type {!security_requirement}. *)

  val string_of_security_requirement :
    ?len:int -> security_requirement -> string
    (** Serialize a value of type {!security_requirement}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_security_requirement :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> security_requirement
    (** Input JSON data of type {!security_requirement}. *)

  val security_requirement_of_string :
    string -> security_requirement
    (** Deserialize JSON data of type {!security_requirement}. *)

  val write_security_definitions :
    Bi_outbuf.t -> security_definitions -> unit
    (** Output a JSON value of type {!security_definitions}. *)

  val string_of_security_definitions :
    ?len:int -> security_definitions -> string
    (** Serialize a value of type {!security_definitions}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_security_definitions :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> security_definitions
    (** Input JSON data of type {!security_definitions}. *)

  val security_definitions_of_string :
    string -> security_definitions
    (** Deserialize JSON data of type {!security_definitions}. *)

  val write_scheme :
    Bi_outbuf.t -> scheme -> unit
    (** Output a JSON value of type {!scheme}. *)

  val string_of_scheme :
    ?len:int -> scheme -> string
    (** Serialize a value of type {!scheme}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_scheme :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> scheme
    (** Input JSON data of type {!scheme}. *)

  val scheme_of_string :
    string -> scheme
    (** Deserialize JSON data of type {!scheme}. *)

  val write_kind :
    Bi_outbuf.t -> kind -> unit
    (** Output a JSON value of type {!kind}. *)

  val string_of_kind :
    ?len:int -> kind -> string
    (** Serialize a value of type {!kind}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_kind :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> kind
    (** Input JSON data of type {!kind}. *)

  val kind_of_string :
    string -> kind
    (** Deserialize JSON data of type {!kind}. *)

  val write_header :
    Bi_outbuf.t -> header -> unit
    (** Output a JSON value of type {!header}. *)

  val string_of_header :
    ?len:int -> header -> string
    (** Serialize a value of type {!header}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_header :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> header
    (** Input JSON data of type {!header}. *)

  val header_of_string :
    string -> header
    (** Deserialize JSON data of type {!header}. *)

  val write_headers :
    Bi_outbuf.t -> headers -> unit
    (** Output a JSON value of type {!headers}. *)

  val string_of_headers :
    ?len:int -> headers -> string
    (** Serialize a value of type {!headers}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_headers :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> headers
    (** Input JSON data of type {!headers}. *)

  val headers_of_string :
    string -> headers
    (** Deserialize JSON data of type {!headers}. *)

  val write_example :
    Bi_outbuf.t -> example -> unit
    (** Output a JSON value of type {!example}. *)

  val string_of_example :
    ?len:int -> example -> string
    (** Serialize a value of type {!example}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_example :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> example
    (** Input JSON data of type {!example}. *)

  val example_of_string :
    string -> example
    (** Deserialize JSON data of type {!example}. *)

  val write_response :
    Bi_outbuf.t -> response -> unit
    (** Output a JSON value of type {!response}. *)

  val string_of_response :
    ?len:int -> response -> string
    (** Serialize a value of type {!response}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_response :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> response
    (** Input JSON data of type {!response}. *)

  val response_of_string :
    string -> response
    (** Deserialize JSON data of type {!response}. *)

  val write_responses_definitions :
    Bi_outbuf.t -> responses_definitions -> unit
    (** Output a JSON value of type {!responses_definitions}. *)

  val string_of_responses_definitions :
    ?len:int -> responses_definitions -> string
    (** Serialize a value of type {!responses_definitions}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_responses_definitions :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> responses_definitions
    (** Input JSON data of type {!responses_definitions}. *)

  val responses_definitions_of_string :
    string -> responses_definitions
    (** Deserialize JSON data of type {!responses_definitions}. *)

  val write_responses :
    Bi_outbuf.t -> responses -> unit
    (** Output a JSON value of type {!responses}. *)

  val string_of_responses :
    ?len:int -> responses -> string
    (** Serialize a value of type {!responses}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_responses :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> responses
    (** Input JSON data of type {!responses}. *)

  val responses_of_string :
    string -> responses
    (** Deserialize JSON data of type {!responses}. *)

  val write_operation :
    Bi_outbuf.t -> operation -> unit
    (** Output a JSON value of type {!operation}. *)

  val string_of_operation :
    ?len:int -> operation -> string
    (** Serialize a value of type {!operation}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_operation :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> operation
    (** Input JSON data of type {!operation}. *)

  val operation_of_string :
    string -> operation
    (** Deserialize JSON data of type {!operation}. *)

  val write_path_item :
    Bi_outbuf.t -> path_item -> unit
    (** Output a JSON value of type {!path_item}. *)

  val string_of_path_item :
    ?len:int -> path_item -> string
    (** Serialize a value of type {!path_item}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_path_item :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> path_item
    (** Input JSON data of type {!path_item}. *)

  val path_item_of_string :
    string -> path_item
    (** Deserialize JSON data of type {!path_item}. *)

  val write_paths :
    Bi_outbuf.t -> paths -> unit
    (** Output a JSON value of type {!paths}. *)

  val string_of_paths :
    ?len:int -> paths -> string
    (** Serialize a value of type {!paths}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_paths :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> paths
    (** Input JSON data of type {!paths}. *)

  val paths_of_string :
    string -> paths
    (** Deserialize JSON data of type {!paths}. *)

  val write_location :
    Bi_outbuf.t -> location -> unit
    (** Output a JSON value of type {!location}. *)

  val string_of_location :
    ?len:int -> location -> string
    (** Serialize a value of type {!location}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_location :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> location
    (** Input JSON data of type {!location}. *)

  val location_of_string :
    string -> location
    (** Deserialize JSON data of type {!location}. *)

  val write_parameter :
    Bi_outbuf.t -> parameter -> unit
    (** Output a JSON value of type {!parameter}. *)

  val string_of_parameter :
    ?len:int -> parameter -> string
    (** Serialize a value of type {!parameter}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_parameter :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> parameter
    (** Input JSON data of type {!parameter}. *)

  val parameter_of_string :
    string -> parameter
    (** Deserialize JSON data of type {!parameter}. *)

  val write_parameters_definitions :
    Bi_outbuf.t -> parameters_definitions -> unit
    (** Output a JSON value of type {!parameters_definitions}. *)

  val string_of_parameters_definitions :
    ?len:int -> parameters_definitions -> string
    (** Serialize a value of type {!parameters_definitions}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_parameters_definitions :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> parameters_definitions
    (** Input JSON data of type {!parameters_definitions}. *)

  val parameters_definitions_of_string :
    string -> parameters_definitions
    (** Deserialize JSON data of type {!parameters_definitions}. *)

  val write_license :
    Bi_outbuf.t -> license -> unit
    (** Output a JSON value of type {!license}. *)

  val string_of_license :
    ?len:int -> license -> string
    (** Serialize a value of type {!license}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_license :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> license
    (** Input JSON data of type {!license}. *)

  val license_of_string :
    string -> license
    (** Deserialize JSON data of type {!license}. *)

  val write_contact :
    Bi_outbuf.t -> contact -> unit
    (** Output a JSON value of type {!contact}. *)

  val string_of_contact :
    ?len:int -> contact -> string
    (** Serialize a value of type {!contact}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_contact :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> contact
    (** Input JSON data of type {!contact}. *)

  val contact_of_string :
    string -> contact
    (** Deserialize JSON data of type {!contact}. *)

  val write_info :
    Bi_outbuf.t -> info -> unit
    (** Output a JSON value of type {!info}. *)

  val string_of_info :
    ?len:int -> info -> string
    (** Serialize a value of type {!info}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_info :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> info
    (** Input JSON data of type {!info}. *)

  val info_of_string :
    string -> info
    (** Deserialize JSON data of type {!info}. *)

  val write_definitions :
    Bi_outbuf.t -> definitions -> unit
    (** Output a JSON value of type {!definitions}. *)

  val string_of_definitions :
    ?len:int -> definitions -> string
    (** Serialize a value of type {!definitions}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_definitions :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> definitions
    (** Input JSON data of type {!definitions}. *)

  val definitions_of_string :
    string -> definitions
    (** Deserialize JSON data of type {!definitions}. *)

  val write_swagger :
    Bi_outbuf.t -> swagger -> unit
    (** Output a JSON value of type {!swagger}. *)

  val string_of_swagger :
    ?len:int -> swagger -> string
    (** Serialize a value of type {!swagger}
        into a JSON string.
        @param len specifies the initial length
                   of the buffer used internally.
                   Default: 1024. *)

  val read_swagger :
    Yojson.Safe.lexer_state -> Lexing.lexbuf -> swagger
    (** Input JSON data of type {!swagger}. *)

  val swagger_of_string :
    string -> swagger
    (** Deserialize JSON data of type {!swagger}. *)
end
