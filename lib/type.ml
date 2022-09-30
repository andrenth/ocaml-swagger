open Ppxlib
open Util

let type_declaration' ~name ~descr ~kind ~manifest =
  let deriving_yojson =
    let name = Ast_builder.Located.mk "deriving" in
    Ast_builder.attribute ~name ~payload:(PStr [%str yojson])
  in
  let name = Ast_builder.Located.mk name in
  let t =
    Ast_builder.type_declaration ~name ~params:[] ~cstrs:[] ~private_:Public
      ~kind ~manifest
  in
  let ptype_attributes =
    match descr with
    | Some descr ->
        Util.ocaml_doc descr :: deriving_yojson :: t.ptype_attributes
    | None -> deriving_yojson :: t.ptype_attributes
  in
  { t with ptype_attributes }

let type_declaration_yojson ~name ~descr =
  let name = Ast_builder.Located.mk name in
  let t =
    Ast_builder.type_declaration ~name ~params:[] ~cstrs:[] ~private_:Public
      ~kind:Ptype_abstract
      ~manifest:(Some [%type: Yojson.Safe.t])
  in
  let ptype_attributes =
    match descr with
    | Some descr -> Util.ocaml_doc descr :: t.ptype_attributes
    | None -> t.ptype_attributes
  in
  { t with ptype_attributes }

module Sig = struct
  type t =
    | Abstract of string * string option
    | Unspecified of string * string option

  let abstract ?descr name = Abstract (name, descr)
  let unspecified ?descr name = Unspecified (name, descr)

  let to_sig t =
    match t with
    | Abstract (name, descr) ->
        let t =
          type_declaration' ~name ~descr ~kind:Ptype_abstract ~manifest:None
        in
        [ Ast_builder.psig_type Recursive [ t ] ]
    | Unspecified (name, descr) ->
        let t = type_declaration_yojson ~name ~descr in
        Ast_builder.psig_type Recursive [ t ]
        :: [%sig:
             val yojson_of_t : t -> Yojson.Safe.t
             val t_of_yojson : t -> Yojson.Safe.t]
end

module Impl = struct
  type record_field = {
    name : string;
    orig_name : string;
    type_ : Ast.core_type;
  }

  type t =
    | Alias of { name : string; target : Ast.core_type; int_or_string : bool }
    | Record of string * record_field list
    | Unspecified of string

  let record_field ~name ~orig_name ~type_ = { name; orig_name; type_ }

  let alias ?(int_or_string = false) name target =
    if int_or_string && target <> [%type: string] then
      invalid_arg "Type.alias: int_or_string only supported for string types";
    Alias { name; target; int_or_string }

  let record name fields = Record (name, fields)
  let unspecified name = Unspecified name

  let yojson_of_int_or_string =
    [%stri
      let yojson_of_t t =
        (* Valid JSON integer. *)
        match Yojson.Safe.from_string t with
        | `Int _ as int -> int
        (* Not a valid JSON integer, use original yojson_of_t function. *)
        | _ -> yojson_of_t t
        | exception Yojson.Json_error _ -> yojson_of_t t]

  let int_or_string_of_yojson =
    [%stri
      let t_of_yojson j =
        match j with `Int _ -> Yojson.Safe.to_string j | _ -> t_of_yojson j]

  let to_impl t =
    match t with
    | Unspecified name ->
        let t = type_declaration_yojson ~name ~descr:None in
        Ast_builder.(pstr_type Recursive [ t ])
        :: [%str
             let t_of_yojson = Fun.id
             let yojson_of_t = Fun.id]
    | Alias { name; target; int_or_string = false } ->
        let t =
          type_declaration' ~name ~descr:None ~kind:Ptype_abstract
            ~manifest:(Some target)
        in
        [ Ast_builder.(pstr_type Recursive [ t ]) ]
    | Alias { name; target; int_or_string = true } ->
        (* Aliases for string types with "int-or-string" format, in addition to
           the automatically generated yojson functions, will generate helper
           wrappers that check if the string value represents a valid integer. If
           that is the case the equivvalent JSON values will be encoded as ints. *)

        (* Only string aliases with int_or_string can be constructed. *)
        assert (target = [%type: string]);

        let t =
          type_declaration' ~name ~descr:None ~kind:Ptype_abstract
            ~manifest:(Some [%type: string])
        in
        let t = Ast_builder.(pstr_type Recursive [ t ]) in
        [ t; yojson_of_int_or_string; int_or_string_of_yojson ]
    | Record (name, []) ->
        let t =
          let name = Ast_builder.Located.mk name in
          Ast_builder.type_declaration ~name ~kind:Ptype_abstract
            ~manifest:(Some [%type: unit])
            ~params:[] ~cstrs:[] ~private_:Public
        in
        Ast_builder.(pstr_type Recursive [ t ])
        :: [%str
             let yojson_of_t () = `Assoc []

             let t_of_yojson = function
               | `Assoc [] | _ -> invalid_arg "Expected empty object"]
    | Record (name, fields) ->
        let labels =
          List.rev_map
            (fun { name; orig_name; type_ } ->
              let label_declaration =
                let name = Ast_builder.Located.mk name in
                Ast_builder.(label_declaration ~name ~mutable_:Immutable ~type_)
              in
              if name = orig_name then label_declaration
              else
                let name = Ast_builder.Located.mk "key" in
                let orig_name = Ast_builder.estring orig_name in
                let attr =
                  Ast_builder.(
                    attribute ~name ~payload:(PStr [%str [%e orig_name]]))
                in
                let pld_attributes = attr :: label_declaration.pld_attributes in
                { label_declaration with pld_attributes })
            fields
        in
        let t =
          type_declaration' ~name ~descr:None ~kind:(Ptype_record labels)
            ~manifest:None
        in
        [ Ast_builder.(pstr_type Recursive [ t ]) ]
end

type t = { signature : Sig.t; implementation : Impl.t }

let create signature implementation = { signature; implementation }

let name t =
  match t.signature with Sig.Abstract (n, _) | Sig.Unspecified (n, _) -> n

let signature t = t.signature
let implementation t = t.implementation

let compose t1 t2 =
  match (t1.signature, t2.signature, t1.implementation, t2.implementation) with
  | ( Sig.Abstract (sig_name1, descr1),
      Sig.Abstract (sig_name2, descr2),
      Impl.Record (impl_name1, fields1),
      Impl.Record (impl_name2, fields2) )
    when sig_name1 = sig_name2 && impl_name1 = impl_name2 && descr1 = descr2 ->
      let impl =
        List.sort_uniq
          (fun { Impl.name = name1; _ } { Impl.name = name2; _ } ->
            String.compare name1 name2)
          (fields1 @ fields2)
      in
      create (Sig.Abstract (sig_name1, descr1)) (Impl.Record (impl_name1, impl))
  | _ -> invalid_arg "Could not compose types. Has the order changed?"
