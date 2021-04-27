(* globalvalidator.ml *)

open StdLabels
open MoreLabels
open Parser
open Util
module Abssyn = Abstractsyntax
module T = Tables

type validator_args = {
  program : Abssyn.program;
  handle_to_class : (T.typ_sym, Abssyn.class_node) Hashtbl.t;
  parents : (T.typ_sym, T.typ_sym) Hashtbl.t;
  sigs : Methodtbl.t;
}

let main_method_exists = ref false

let is_valid_type ~parents ~typ =
  lazy (typ = T.object_type || Hashtbl.mem parents typ)

let check_field_type ~parents
    ~field_var:{ Abssyn.elem = field_id, field_typ; loc } =
  Validator.bind
    ~checker:(is_valid_type ~parents ~typ:field_typ)
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc "Class %a of attribute %a is undefined."
           T.print_type field_typ T.print_id field_id))
    true

let check_formal ~parents { Abssyn.elem = formal_id, formal_typ; loc } =
  Validator.bind
    ~checker:(is_valid_type ~parents ~typ:formal_typ)
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc
           "Class %a of formal parameter %a is undefined." T.print_type
           formal_typ T.print_id formal_id))
    true

let check_ret_type ~parents ~method_def ~loc =
  let ret_typ = method_def.Abssyn.method_ret_typ in
  Validator.bind
    ~checker:(is_valid_type ~parents ~typ:ret_typ)
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc "Undefined return type %a in method %a."
           T.print_type ret_typ T.print_id method_def.method_id))
    true

let check_method ~parents ~method_def ~loc =
  let valid_formals =
    List.for_all ~f:(check_formal ~parents) method_def.Abssyn.method_formals
  in
  check_ret_type ~parents ~method_def ~loc && valid_formals

let check_feature ~parents feature =
  let loc = feature.Abssyn.loc in
  match feature.elem with
  | Abssyn.Method method_def -> check_method ~parents ~method_def ~loc
  | Abssyn.Field { field_var; _ } -> check_field_type ~parents ~field_var

let validate_feature_types ~parents (cl : Abssyn.class_node) =
  List.for_all ~f:(check_feature ~parents) cl.elem.cl_features

let validate_main_method ~typ ~method_def ~loc =
  let method_id = method_def.Abssyn.method_id in
  let is_main_method = typ = T.main_type && method_id = T.main_method in
  (not is_main_method)
  ||
  let _ = main_method_exists := true in
  Validator.bind
    ~checker:
      (lazy (List.compare_length_with method_def.method_formals ~len:0 = 0))
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc
           "'main' method in class Main should have no arguments."))
    true

let add_to_sigs sigs ~typ ~method_def ~loc =
  let formals =
    List.map
      ~f:(fun formal -> formal.Abssyn.elem)
      method_def.Abssyn.method_formals
  in
  let method_id = method_def.method_id in
  let ret_typ = method_def.method_ret_typ in
  Validator.bind
    ~checker:(lazy (Methodtbl.add sigs ~typ ~method_id ~ret_typ ~formals))
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc "Method %a is multiply defined."
           T.print_id method_id))
    true

let validate_formal_not_self { Abssyn.elem = id, typ; loc } =
  Validator.bind
    ~checker:(Validator.is_not_self_var ~id)
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc
           "'self' cannot be the name of a formal parameter."))
    true
  |> Validator.bind
       ~checker:(Validator.is_not_self_type ~typ)
       ~err_fun:
         (lazy
           (Semantprint.print_error ~loc
              "Formal parameter %a cannot have type SELF_TYPE." T.print_id id))

let validate_method ~sigs ~method_def ~typ ~loc =
  let is_unique = add_to_sigs sigs ~typ ~method_def ~loc in
  let is_valid_main = validate_main_method ~typ ~method_def ~loc in
  List.for_all ~f:validate_formal_not_self method_def.Abssyn.method_formals
  && is_unique && is_valid_main

let validate_field_not_self ~field_var:{ Abssyn.elem = field_id, _; loc } =
  Validator.bind
    ~checker:(Validator.is_not_self_var ~id:field_id)
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc
           "'self' cannot be the name of an attribute."))
    true

let validate_feature ~args ~typ feature =
  let loc = feature.Abssyn.loc in
  match feature.elem with
  | Abssyn.Field { field_var; _ } -> validate_field_not_self ~field_var
  | Abssyn.Method method_def ->
      validate_method ~sigs:args.sigs ~method_def ~typ ~loc

let add_to_parents ~parents ~cl:{ Abssyn.elem; loc } =
  let typ = elem.Abssyn.cl_typ in
  Validator.bind
    ~accept:(lazy (Hashtbl.add parents ~key:typ ~data:elem.cl_parent))
    ~checker:(lazy (Hashtbl.mem parents typ |> not))
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc "Class %a was previously defined."
           T.print_type typ))
    true

let is_valid_inheritance ~cl:{ Abssyn.elem; loc } =
  Validator.bind
    ~checker:
      (lazy (Hashtbl.mem T.inheritance_blocklist elem.Abssyn.cl_parent |> not))
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc "Class %a cannot inherit class %a."
           T.print_type elem.cl_typ T.print_type elem.cl_parent))
    true

let is_unreserved ~cl:{ Abssyn.elem; loc } =
  Validator.bind
    ~checker:(lazy (Hashtbl.mem T.reserved_classes elem.Abssyn.cl_typ |> not))
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc "Redefinition of basic class %a."
           T.print_type elem.cl_typ))
    true

let validate_class ~args (cl : Abssyn.class_node) =
  let typ = cl.elem.cl_typ in
  Hashtbl.add args.handle_to_class ~key:typ ~data:cl;
  let is_valid_type = is_unreserved ~cl in
  let is_valid_parent = is_valid_inheritance ~cl in
  let is_unique = is_valid_type && add_to_parents ~parents:args.parents ~cl in
  List.for_all ~f:(validate_feature ~args ~typ) cl.elem.cl_features
  && is_valid_type && is_valid_parent && is_unique

let validate_main_method_exists ~loc =
  Validator.bind
    ~checker:(lazy !main_method_exists)
    ~err_fun:
      (lazy (Semantprint.print_error ~loc "No 'main' method in class Main."))
    true

let validate_main ~handle_to_class =
  match Hashtbl.find_opt handle_to_class T.main_type with
  | Some main_cl -> validate_main_method_exists ~loc:main_cl.Abssyn.loc
  | None ->
      Format.eprintf "@[Class Main is not defined.@]@.";
      false

let create_tree ~parents ~handle_to_class =
  match Tree.create ~parents ~root:T.object_type with
  | Tree.Tree tree -> Some tree
  | Tree.Disconnected (typ, parent) ->
      let cl = Hashtbl.find handle_to_class typ in
      Semantprint.print_error ~loc:cl.Abssyn.loc
        "Class %a inherits from an undefined class %a." T.print_type typ
        T.print_type parent;
      None
  | Tree.Cycle typ ->
      let cl = Hashtbl.find handle_to_class typ in
      Semantprint.print_error ~loc:cl.Abssyn.loc
        "Class %a, or an ancestor of %a, is involved in an inheritance cycle."
        T.print_type typ T.print_type typ;
      None

let validate ~args =
  let classes = args.program.Abssyn.elem in
  let valid_classes = List.for_all ~f:(validate_class ~args) classes in
  let handle_to_class = args.handle_to_class in
  let is_valid_main = valid_classes && validate_main ~handle_to_class in
  let parents = args.parents in
  let tree_opt =
    if valid_classes then create_tree ~parents ~handle_to_class else None
  in
  let valid_feature_types =
    List.for_all ~f:(validate_feature_types ~parents) classes
  in
  (valid_classes && valid_feature_types && is_valid_main, tree_opt)
