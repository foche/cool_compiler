(* globalvalidator.ml *)

open StdLabels
open MoreLabels
open Parser
open Util
module Abssyn = Abstractsyntax
module T = Tables

type validator_args = {
  program : Abssyn.program;
  handle_to_class : (T.type_sym, Abssyn.class_node) Hashtbl.t;
  graph : (T.type_sym, T.type_sym) Hashtbl.t;
  sigs : Methodtbl.t;
}

let main_method_exists = ref false

let validate_feature_types ~graph (cl : Abssyn.class_node) =
  let is_valid_type ~typ = typ = T.object_type || Hashtbl.mem graph typ in
  let check_field_type ~id ~typ ~loc =
    match is_valid_type ~typ with
    | true -> true
    | false ->
        Semantprint.print_location loc;
        Printf.eprintf "Class %a of attribute %a is undefined.\n" T.print_type
          typ T.print_id id;
        false
  in
  let check_formal formal =
    let id, typ = formal.Abssyn.elem in
    match is_valid_type ~typ with
    | true -> true
    | false ->
        Semantprint.print_location formal.loc;
        Printf.eprintf "Class %a of formal parameter %a is undefined.\n"
          T.print_type typ T.print_id id;
        false
  in
  let check_ret_type ~method_def ~loc =
    match is_valid_type ~typ:method_def.Abssyn.method_ret_typ with
    | true -> true
    | false ->
        Semantprint.print_location loc;
        Printf.eprintf "Undefined return type %a in method %a.\n" T.print_type
          method_def.method_ret_typ T.print_id method_def.method_id;
        false
  in
  let check_method ~method_def ~loc =
    let valid_ret_typ = check_ret_type ~method_def ~loc in
    let valid_formals =
      List.for_all ~f:check_formal method_def.Abssyn.method_formals
    in
    valid_ret_typ && valid_formals
  in
  let check_feature feature =
    let loc = feature.Abssyn.loc in
    match feature.elem with
    | Abssyn.Method method_def -> check_method ~method_def ~loc
    | Abssyn.Field ((id, typ), _) -> check_field_type ~id ~typ ~loc
  in
  List.for_all ~f:check_feature cl.elem.cl_features

let validate_formal_not_self formal =
  let id, typ = formal.Abssyn.elem in
  let is_valid_id =
    match id <> T.self_var with
    | true -> true
    | false ->
        Semantprint.print_location formal.loc;
        prerr_endline "'self' cannot be the name of a formal parameter.";
        false
  in
  let is_valid_type =
    match typ <> T.self_type with
    | true -> true
    | false ->
        Semantprint.print_location formal.loc;
        Printf.eprintf "Formal parameter %a cannot have type SELF_TYPE.\n"
          T.print_id id;
        false
  in
  is_valid_id && is_valid_type

let validate_main_method ~typ ~method_def ~loc =
  let method_id = method_def.Abssyn.method_id in
  let is_main_method = typ = T.main_type && method_id = T.main_method in
  match is_main_method with
  | false -> true
  | true -> (
      main_method_exists := true;
      match List.compare_length_with method_def.method_formals ~len:0 with
      | 0 -> true
      | _ ->
          Semantprint.print_location loc;
          prerr_endline "'main' method in class Main should have no arguments.";
          false)

let add_to_sigs sigs ~typ ~method_def ~loc =
  let formals =
    List.map
      ~f:(fun formal -> formal.Abssyn.elem)
      method_def.Abssyn.method_formals
  in
  let method_id = method_def.method_id in
  let ret_typ = method_def.method_ret_typ in
  let is_unique = Methodtbl.add sigs ~typ ~method_id ~ret_typ ~formals in
  (match is_unique with
  | true -> ()
  | false ->
      Semantprint.print_location loc;
      Printf.eprintf "Method %a is multiply defined.\n" T.print_id
        method_def.method_id);
  is_unique

let validate_field_not_self ~loc ~id =
  match id <> T.self_var with
  | true -> true
  | false ->
      Semantprint.print_location loc;
      prerr_endline "'self' cannot be the name of an attribute.";
      false

let validate_feature ~args ~(cl : Abssyn.class_node) feature =
  let loc = feature.Abssyn.loc in
  let typ = cl.elem.cl_typ in
  match feature.elem with
  | Abssyn.Field ((id, _), _) -> validate_field_not_self ~loc ~id
  | Abssyn.Method method_def ->
      let is_unique = add_to_sigs args.sigs ~typ ~method_def ~loc in
      let is_valid_main = validate_main_method ~typ ~method_def ~loc in
      let no_self_args =
        List.for_all ~f:validate_formal_not_self
          method_def.Abssyn.method_formals
      in
      is_unique && is_valid_main && no_self_args

let add_to_graph ~graph ~(cl : Abssyn.class_node) =
  match Hashtbl.mem graph cl.elem.cl_typ with
  | false ->
      Hashtbl.add graph ~key:cl.elem.cl_typ ~data:cl.elem.cl_parent;
      true
  | true ->
      Semantprint.print_location cl.loc;
      Printf.eprintf "Class %a was previously defined.\n" T.print_type
        cl.elem.cl_typ;
      false

let is_valid_inheritance ~(cl : Abssyn.class_node) =
  match Hashtbl.mem T.inheritance_blocklist cl.elem.cl_parent with
  | false -> true
  | true ->
      Semantprint.print_location cl.loc;
      Printf.eprintf "Class %a cannot inherit class %a.\n" T.print_type
        cl.elem.cl_typ T.print_type cl.elem.cl_parent;
      false

let is_unreserved ~(cl : Abssyn.class_node) =
  match Hashtbl.mem T.reserved_classes cl.elem.cl_typ with
  | false -> true
  | true ->
      Semantprint.print_location cl.loc;
      Printf.eprintf "Redefinition of basic class %a.\n" T.print_type
        cl.elem.cl_typ;
      false

let validate_class ~args (cl : Abssyn.class_node) =
  Hashtbl.add args.handle_to_class ~key:cl.elem.cl_typ ~data:cl;
  let is_valid_type = is_unreserved ~cl in
  let is_valid_parent = is_valid_inheritance ~cl in
  let graph = args.graph in
  let is_unique = is_valid_type && add_to_graph ~graph ~cl in
  let valid_features =
    List.for_all ~f:(validate_feature ~args ~cl) cl.elem.cl_features
  in
  is_valid_type && is_valid_parent && is_unique && valid_features

let validate_main_method_exists ~loc =
  match !main_method_exists with
  | true -> true
  | false ->
      Semantprint.print_location loc;
      prerr_endline "No 'main' method in class Main.";
      false

let validate_main ~handle_to_class =
  match Hashtbl.find_opt handle_to_class T.main_type with
  | Some main_cl -> validate_main_method_exists ~loc:main_cl.Abssyn.loc
  | None ->
      prerr_endline "Class Main is not defined.";
      false

let create_tree ~graph ~handle_to_class =
  match Tree.create ~parents:graph ~root:T.object_type with
  | Tree.Tree tree -> (true, Some tree)
  | Tree.Disconnected (typ, parent) ->
      let cl = Hashtbl.find handle_to_class typ in
      Semantprint.print_location cl.Abssyn.loc;
      Printf.eprintf "Class %a inherits from an undefined class %a.\n"
        T.print_type typ T.print_type parent;
      (false, None)
  | Tree.Cycle typ ->
      let cl = Hashtbl.find handle_to_class typ in
      Semantprint.print_location cl.Abssyn.loc;
      Printf.eprintf
        "Class %a, or an ancestor of %a, is involved in an inheritance cycle.\n"
        T.print_type typ T.print_type typ;
      (false, None)

let validate ~args =
  let classes = args.program.Abssyn.elem in
  let valid_classes = List.for_all ~f:(validate_class ~args) classes in
  let handle_to_class = args.handle_to_class in
  let graph = args.graph in
  let valid_main = validate_main ~handle_to_class in
  let valid_tree, tree_opt =
    match valid_classes with
    | false -> (false, None)
    | true -> create_tree ~graph ~handle_to_class
  in
  let valid_method_types =
    List.for_all ~f:(validate_feature_types ~graph) classes
  in
  (valid_classes && valid_tree && valid_method_types && valid_main, tree_opt)
