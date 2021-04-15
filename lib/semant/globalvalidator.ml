(* globalvalidator.ml *)

open Parser
open Ast
open Util
open Tables

type validator_args =
  { program: program
  ; reserved_classes: (type_sym, unit) Hashtbl.t
  ; inheritance_blocklist: (type_sym, unit) Hashtbl.t
  ; handle_to_class: (type_sym, class_node) Hashtbl.t
  ; graph: (Tables.type_sym, Tables.type_sym) Hashtbl.t
  ; sigs: Methodtbl.t }

let main_method_exists = ref false

let validate_method_types graph (cl, _) =
  let is_valid_type typ = typ = object_type || Hashtbl.mem graph typ in
  let check_field_type line_number id typ =
    match is_valid_type typ with
    | true -> true
    | false ->
        Semantprint.print_location cl.class_filename line_number ;
        Printf.eprintf "Class %a of attribute %a is undefined.\n" print_type
          typ print_id id ;
        false
  in
  let check_formal ((id, typ), line_number) =
    match is_valid_type typ with
    | true -> true
    | false ->
        Semantprint.print_location cl.class_filename line_number ;
        Printf.eprintf "Class %a of formal parameter %a is undefined.\n"
          print_type typ print_id id ;
        false
  in
  let check_ret_type typ line_number method_id =
    match is_valid_type typ with
    | true -> true
    | false ->
        Semantprint.print_location cl.class_filename line_number ;
        Printf.eprintf "Undefined return type %a in method %a.\n" print_type
          typ print_id method_id ;
        false
  in
  let check_method mthd line_number =
    let valid_ret_type =
      check_ret_type mthd.method_ret_type line_number mthd.method_id
    in
    let valid_formals = List.for_all check_formal mthd.method_args in
    valid_ret_type && valid_formals
  in
  let check_feature (feat, line_number) =
    match feat with
    | Method mthd -> check_method mthd line_number
    | Field (id, typ, _) -> check_field_type line_number id typ
  in
  List.for_all check_feature cl.class_features

let validate_formal_not_self filename ((id, typ), line_number) =
  let is_valid_id =
    match id <> self_var with
    | true -> true
    | false ->
        Semantprint.print_location filename line_number ;
        prerr_endline "'self' cannot be the name of a formal parameter." ;
        false
  in
  let is_valid_type =
    match typ <> self_type with
    | true -> true
    | false ->
        Semantprint.print_location filename line_number ;
        Printf.eprintf "Formal parameter %a cannot have type SELF_TYPE.\n"
          print_id id ;
        false
  in
  is_valid_id && is_valid_type

let validate_main_method cl mthd line_number =
  let typ = cl.class_type in
  let method_id = mthd.method_id in
  let is_main_method = typ = main_type && method_id = main_method in
  match is_main_method with
  | false -> true
  | true -> (
      main_method_exists := true ;
      match List.compare_length_with mthd.method_args 0 with
      | 0 -> true
      | _ ->
          Semantprint.print_location cl.class_filename line_number ;
          prerr_endline "'main' method in class Main should have no arguments." ;
          false )

let add_to_sigs sigs cl mthd line_number =
  let formals = List.map fst mthd.method_args in
  let is_unique =
    Methodtbl.add ~tbl:sigs ~clazz:cl.class_type ~method_id:mthd.method_id
      ~return_type:mthd.method_ret_type ~formals
  in
  ( match is_unique with
  | true -> ()
  | false ->
      Semantprint.print_location cl.class_filename line_number ;
      Printf.eprintf "Method %a is multiply defined.\n" print_id mthd.method_id
  ) ;
  is_unique

let validate_field_not_self cl line_number id =
  match id <> self_var with
  | true -> true
  | false ->
      Semantprint.print_location cl.class_filename line_number ;
      prerr_endline "'self' cannot be the name of an attribute." ;
      false

let validate_feature args cl (feat, line_number) =
  match feat with
  | Field (id, _, _) -> validate_field_not_self cl line_number id
  | Method mthd ->
      let is_unique = add_to_sigs args.sigs cl mthd line_number in
      let is_valid_main = validate_main_method cl mthd line_number in
      let no_self_args =
        List.for_all
          (validate_formal_not_self cl.class_filename)
          mthd.method_args
      in
      is_unique && is_valid_main && no_self_args

let add_to_graph graph cl line_number =
  match Hashtbl.mem graph cl.class_type with
  | false ->
      Hashtbl.add graph cl.class_type cl.class_parent ;
      true
  | true ->
      Semantprint.print_location cl.class_filename line_number ;
      Printf.eprintf "Class %a was previously defined.\n" print_type
        cl.class_type ;
      false

let is_valid_inheritance inheritance_blocklist cl line_number =
  match Hashtbl.mem inheritance_blocklist cl.class_parent with
  | false -> true
  | true ->
      Semantprint.print_location cl.class_filename line_number ;
      Printf.eprintf "Class %a cannot inherit class %a.\n" print_type
        cl.class_type print_type cl.class_parent ;
      false

let is_unreserved reserved_classes cl line_number =
  match Hashtbl.mem reserved_classes cl.class_type with
  | false -> true
  | true ->
      Semantprint.print_location cl.class_filename line_number ;
      Printf.eprintf "Redefinition of basic class %a.\n" print_type
        cl.class_type ;
      false

let validate_class args ((cl, line_number) as cl_node) =
  Hashtbl.add args.handle_to_class cl.class_type cl_node ;
  let is_valid_type = is_unreserved args.reserved_classes cl line_number in
  let is_valid_parent =
    is_valid_inheritance args.inheritance_blocklist cl line_number
  in
  let is_unique = is_valid_type && add_to_graph args.graph cl line_number in
  let valid_features =
    List.for_all (validate_feature args cl) cl.class_features
  in
  is_valid_type && is_valid_parent && is_unique && valid_features

let validate_main_method_exists filename line_number =
  match !main_method_exists with
  | true -> true
  | false ->
      Semantprint.print_location filename line_number ;
      prerr_endline "No 'main' method in class Main." ;
      false

let validate_main handle_to_class =
  match Hashtbl.find_opt handle_to_class main_type with
  | Some (main_cl, line_number) ->
      validate_main_method_exists main_cl.class_filename line_number
  | None ->
      prerr_endline "Class Main is not defined." ;
      false

let create_tree graph handle_to_class =
  match Tree.create ~parents:graph ~root:object_type with
  | Tree.Tree tree -> (true, Some tree)
  | Tree.Disconnected (typ, parent) ->
      let cl, line_number = Hashtbl.find handle_to_class typ in
      Semantprint.print_location cl.class_filename line_number ;
      Printf.eprintf "Class %a inherits from an undefined class %a.\n"
        print_type typ print_type parent ;
      (false, None)
  | Tree.Cycle typ ->
      let cl, line_number = Hashtbl.find handle_to_class typ in
      Semantprint.print_location cl.class_filename line_number ;
      Printf.eprintf
        "Class %a, or an ancestor of %a, is involved in an inheritance cycle.\n"
        print_type typ print_type typ ;
      (false, None)

let validate args =
  let classes = fst args.program in
  let valid_classes = List.for_all (validate_class args) classes in
  let valid_main = validate_main args.handle_to_class in
  let valid_tree, tree_opt =
    match valid_classes with
    | false -> (false, None)
    | true -> create_tree args.graph args.handle_to_class
  in
  let valid_method_types =
    List.for_all (validate_method_types args.graph) classes
  in
  (valid_classes && valid_tree && valid_method_types && valid_main, tree_opt)
