(* localvalidator.ml *)

open StdLabels
open MoreLabels
open Parser
open Util
module Abssyn = Abstractsyntax
module T = Tables

type validator_args = {
  id_env : (T.id_sym, T.type_sym) Symtbl.t;
  func_env : (T.id_sym, Abssyn.method_def) Symtbl.t;
  inherit_tree : T.type_sym Tree.t;
  sigs : Methodtbl.t;
  untyped_classes : (T.type_sym, Abssyn.class_node) Hashtbl.t;
  typed_classes : (T.type_sym, Abssyn.class_node) Hashtbl.t;
}

let typecheck_field ~args ~cl_typ ~feature ~id ~typ ~init =
  match init.Abssyn.expr_expr with
  | NoExpr -> Some feature
  | _ -> (
      let typed_init =
        Exprchecker.typecheck
          ~ctx:
            {
              id_env = args.id_env;
              sigs = args.sigs;
              inherit_tree = args.inherit_tree;
              cl_typ;
            }
          ~expr:init
      in
      match typed_init.Abssyn.expr_typ with
      | None -> None
      | Some init_typ -> (
          match
            Types.is_subtype args.inherit_tree ~cl_typ ~sub_typ:init_typ
              ~super_typ:typ
          with
          | true ->
              Some
                {
                  feature with
                  Abssyn.elem = Abssyn.Field ((id, typ), typed_init);
                }
          | false ->
              Semantprint.print_location feature.loc;
              Printf.eprintf
                "Inferred type %a of initialization of attribute %a does not \
                 conform to declared type %a.\n"
                T.print_type init_typ T.print_id id T.print_type typ;
              None))

let typecheck_method ~args ~cl_typ ~method_def ~feature =
  let add_formal formal =
    let id, typ = formal.Abssyn.elem in
    let is_duplicate, _ = Symtbl.add args.id_env ~key:id ~data:typ in
    match is_duplicate with
    | false -> true
    | true ->
        Semantprint.print_location formal.loc;
        Printf.eprintf "Formal parameter %a is multiply defined.\n" T.print_id
          id;
        false
  in
  let lazy_typecheck =
    lazy
      (match List.for_all ~f:add_formal method_def.Abssyn.method_formals with
      | false -> method_def.method_body
      | true ->
          Exprchecker.typecheck
            ~ctx:
              {
                id_env = args.id_env;
                sigs = args.sigs;
                inherit_tree = args.inherit_tree;
                cl_typ;
              }
            ~expr:method_def.method_body)
  in
  let typed_body = Symtbl.enter_scope args.id_env ~cont:lazy_typecheck in
  match typed_body.Abssyn.expr_typ with
  | None -> None
  | Some body_typ -> (
      match
        Types.is_subtype args.inherit_tree ~cl_typ ~sub_typ:body_typ
          ~super_typ:method_def.method_ret_typ
      with
      | true ->
          Some
            {
              feature with
              Abssyn.elem =
                Abssyn.Method { method_def with method_body = typed_body };
            }
      | false ->
          Semantprint.print_location feature.loc;
          Printf.eprintf
            "Inferred return type %a of method %a does not conform to declared \
             return type %a.\n"
            T.print_type body_typ T.print_id method_def.method_id T.print_type
            method_def.method_ret_typ;
          None)

let typecheck_feature ~args ~cl_typ feature =
  match feature.Abssyn.elem with
  | Abssyn.Field ((id, typ), init) ->
      typecheck_field ~args ~cl_typ ~feature ~id ~typ ~init
  | Abssyn.Method method_def ->
      typecheck_method ~args ~cl_typ ~method_def ~feature

let validate_formal_types ~method_id formal parent_formal =
  let _, typ = formal.Abssyn.elem in
  let _, parent_typ = parent_formal.Abssyn.elem in
  match typ = parent_typ with
  | true -> true
  | false ->
      Semantprint.print_location formal.loc;
      Printf.eprintf
        "In redefined method %a, parameter type %a is different from original \
         type %a.\n"
        T.print_id method_id T.print_type typ T.print_type parent_typ;
      false

let validate_overridden_method ~method_def ~loc parent_method =
  let param_counts_match =
    List.compare_lengths method_def.Abssyn.method_formals
      parent_method.Abssyn.method_formals
    = 0
  in
  let return_types_match =
    method_def.method_ret_typ = parent_method.method_ret_typ
  in
  let formal_types_match =
    param_counts_match
    && List.for_all2
         ~f:(validate_formal_types ~method_id:method_def.method_id)
         method_def.method_formals parent_method.method_formals
  in
  (match param_counts_match with
  | true -> ()
  | false ->
      Semantprint.print_location loc;
      Printf.eprintf
        "Incompatible number of formal parameters in redefined method %a.\n"
        T.print_id method_def.method_id);
  (match return_types_match with
  | true -> ()
  | false ->
      Semantprint.print_location loc;
      Printf.eprintf
        "In redefined method %a, return type %a is different from original \
         return type %a.\n"
        T.print_id method_def.method_id T.print_type method_def.method_ret_typ
        T.print_type parent_method.method_ret_typ);
  param_counts_match && return_types_match && formal_types_match

let extract_method ~func_env ~method_def ~loc =
  let parent_method_opt =
    Symtbl.find_opt func_env method_def.Abssyn.method_id
  in
  let _, is_overridden =
    Symtbl.add func_env ~key:method_def.method_id ~data:method_def
  in
  match is_overridden with
  | false -> true
  | true ->
      Option.get parent_method_opt
      |> validate_overridden_method ~method_def ~loc

let extract_field ~id_env ~cl_typ ~loc ~id ~typ =
  let is_duplicate, is_shadowed = Symtbl.add id_env ~key:id ~data:typ in
  (match is_duplicate with
  | false -> ()
  | true ->
      Semantprint.print_location loc;
      Printf.eprintf "Attribute %a is multiply defined in class %a.\n"
        T.print_id id T.print_type cl_typ);
  (match is_shadowed with
  | false -> ()
  | true ->
      Semantprint.print_location loc;
      Printf.eprintf "Attribute %a is an attribute of an inherited class.\n"
        T.print_id id);
  (not is_duplicate) && not is_shadowed

let extract_feature ~id_env ~func_env ~cl_typ feature =
  let loc = feature.Abssyn.loc in
  match feature.elem with
  | Abssyn.Method method_def -> extract_method ~func_env ~method_def ~loc
  | Abssyn.Field ((id, typ), _) -> extract_field ~id_env ~cl_typ ~loc ~id ~typ

let typecheck_class ~args ~typ (cl : Abssyn.class_node) =
  let cl_typ = cl.elem.cl_typ in
  let features = cl.elem.cl_features in
  let valid_field_decl =
    List.for_all
      ~f:(extract_feature ~id_env:args.id_env ~func_env:args.func_env ~cl_typ)
      features
  in
  match valid_field_decl with
  | false -> false
  | true -> (
      let typed_feature_opt =
        List.rev_map ~f:(typecheck_feature ~args ~cl_typ) features
        |> Optutil.flatten_opt_list
      in
      match typed_feature_opt with
      | None -> false
      | Some typed_features ->
          let typed_class =
            { cl with elem = { cl.elem with cl_features = typed_features } }
          in
          Hashtbl.replace args.typed_classes ~key:typ ~data:typed_class;
          true)

let rec traverse_class_tree ~args root =
  Symtbl.enter_scope args.id_env
    ~cont:
      (lazy
        (Symtbl.enter_scope args.func_env
           ~cont:
             (lazy
               ((Hashtbl.mem T.reserved_classes root
                || Hashtbl.find args.untyped_classes root
                   |> typecheck_class ~args ~typ:root)
               && Tree.find_out_edges args.inherit_tree root
                  |> List.for_all ~f:(traverse_class_tree ~args)))))

let validate ~args =
  Symtbl.add args.id_env ~key:T.self_var ~data:T.self_type |> ignore;
  traverse_class_tree ~args T.object_type
