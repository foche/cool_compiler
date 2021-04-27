(* localvalidator.ml *)

open StdLabels
open MoreLabels
open Parser
open Util
module Abssyn = Abstractsyntax
module T = Tables

type validator_args = {
  id_env : (T.id_sym, T.typ_sym) Symtbl.t;
  func_env : (T.id_sym, Abssyn.method_def) Symtbl.t;
  inherit_tree : T.typ_sym Tree.t;
  sigs : Methodtbl.t;
  untyped_classes : (T.typ_sym, Abssyn.class_node) Hashtbl.t;
  typed_classes : (T.typ_sym, Abssyn.class_node) Hashtbl.t;
}

let check_field_init ~args ~feature ~cl_typ
    ~field_def:({ Abssyn.field_var; _ } as field_def) ~typed_init =
  match typed_init.Abssyn.expr_typ with
  | None -> None
  | Some init_typ ->
      let field_id, field_typ = field_var.Abssyn.elem in
      let is_valid_init_type =
        Types.is_subtype args.inherit_tree ~cl_typ ~sub_typ:init_typ
          ~super_typ:field_typ
      in
      if is_valid_init_type then
        Some
          {
            feature with
            Abssyn.elem =
              Abssyn.Field { field_def with field_init = typed_init };
          }
      else (
        Semantprint.print_error ~loc:typed_init.expr_loc
          "Inferred type %a of initialization of attribute %a does not conform \
           to declared type %a."
          T.print_type init_typ T.print_id field_id T.print_type field_typ;
        None)

let typecheck_field ~args ~cl_typ ~feature
    ~field_def:({ Abssyn.field_var; field_init } as field_def) =
  let _, field_typ = field_var.elem in
  let typed_init =
    Exprchecker.typecheck ~super_typ:field_typ
      ~ctx:
        {
          id_env = args.id_env;
          sigs = args.sigs;
          inherit_tree = args.inherit_tree;
          cl_typ;
        }
      field_init
  in
  check_field_init ~args ~feature ~cl_typ ~field_def ~typed_init

let check_method_body ~inherit_tree ~cl_typ ~feature ~method_def ~typed_body =
  match typed_body.Abssyn.expr_typ with
  | None -> None
  | Some body_typ ->
      let is_valid_body_type =
        Types.is_subtype inherit_tree ~cl_typ ~sub_typ:body_typ
          ~super_typ:method_def.Abssyn.method_ret_typ
      in
      if is_valid_body_type then
        Some
          {
            feature with
            Abssyn.elem =
              Abssyn.Method { method_def with method_body = typed_body };
          }
      else (
        Semantprint.print_error ~loc:typed_body.expr_loc
          "Inferred return type %a of method %a does not conform to declared \
           return type %a."
          T.print_type body_typ T.print_id method_def.method_id T.print_type
          method_def.method_ret_typ;
        None)

let typecheck_method ~args ~cl_typ ~feature ~method_def =
  let lazy_typecheck =
    lazy
      (let add_formal { Abssyn.elem = id, typ; loc } =
         Validator.bind
           ~checker:
             (lazy (Symtbl.add args.id_env ~key:id ~data:typ |> fst |> not))
           ~err_fun:
             (lazy
               (Semantprint.print_error ~loc
                  "Formal parameter %a is multiply defined." T.print_id id))
           true
       in
       if List.for_all ~f:add_formal method_def.Abssyn.method_formals then
         Exprchecker.typecheck
           ~ctx:
             {
               id_env = args.id_env;
               sigs = args.sigs;
               inherit_tree = args.inherit_tree;
               cl_typ;
             }
           method_def.method_body
       else method_def.method_body)
  in
  let typed_body = Symtbl.enter_scope args.id_env ~cont:lazy_typecheck in
  check_method_body ~inherit_tree:args.inherit_tree ~cl_typ ~feature ~method_def
    ~typed_body

let typecheck_feature ~args ~cl_typ feature =
  match feature.Abssyn.elem with
  | Abssyn.Field field_def -> typecheck_field ~args ~cl_typ ~feature ~field_def
  | Abssyn.Method method_def ->
      typecheck_method ~args ~cl_typ ~feature ~method_def

let validate_formal_types ~method_id formal parent_formal =
  let _, typ = formal.Abssyn.elem in
  let _, parent_typ = parent_formal.Abssyn.elem in
  Validator.bind
    ~checker:(lazy (typ = parent_typ))
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc:formal.loc
           "In redefined method %a, parameter type %a is different from \
            original type %a."
           T.print_id method_id T.print_type typ T.print_type parent_typ))
    true

let validate_overridden_method ~method_def ~loc ~overridden_method =
  Validator.bind
    ~checker:
      (lazy
        (List.compare_lengths method_def.Abssyn.method_formals
           overridden_method.Abssyn.method_formals
        = 0))
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc
           "Incompatible number of formal parameters in redefined method %a."
           T.print_id method_def.method_id))
    true
  |> Validator.bind
       ~checker:
         (lazy (method_def.method_ret_typ = overridden_method.method_ret_typ))
       ~err_fun:
         (lazy
           (Semantprint.print_error ~loc
              "In redefined method %a, return type %a is different from \
               original return type %a."
              T.print_id method_def.method_id T.print_type
              method_def.method_ret_typ T.print_type
              overridden_method.method_ret_typ))
  && List.for_all2
       ~f:(validate_formal_types ~method_id:method_def.method_id)
       method_def.method_formals overridden_method.method_formals

let extract_method ~func_env ~method_def ~loc =
  let overridden_method_opt =
    Symtbl.find_opt func_env method_def.Abssyn.method_id
  in
  Symtbl.add func_env ~key:method_def.method_id ~data:method_def |> ignore;
  match overridden_method_opt with
  | None -> true
  | Some overridden_method ->
      validate_overridden_method ~method_def ~loc ~overridden_method

let extract_field ~id_env ~cl_typ
    ~field_var:{ Abssyn.elem = field_id, field_typ; loc } =
  let is_duplicate, is_shadowed =
    Symtbl.add id_env ~key:field_id ~data:field_typ
  in
  if is_duplicate then
    Semantprint.print_error ~loc "Attribute %a is multiply defined in class %a."
      T.print_id field_id T.print_type cl_typ;
  if is_shadowed then
    Semantprint.print_error ~loc
      "Attribute %a is an attribute of an inherited class." T.print_id field_id;
  (not is_duplicate) && not is_shadowed

let extract_feature ~id_env ~func_env ~cl_typ feature =
  match feature.Abssyn.elem with
  | Abssyn.Method method_def ->
      extract_method ~func_env ~method_def ~loc:feature.loc
  | Abssyn.Field { Abssyn.field_var; _ } ->
      extract_field ~id_env ~cl_typ ~field_var

let typecheck_class ~args ~typ (cl : Abssyn.class_node) =
  let cl_typ = cl.elem.cl_typ in
  let features = cl.elem.cl_features in
  List.for_all
    ~f:(extract_feature ~id_env:args.id_env ~func_env:args.func_env ~cl_typ)
    features
  &&
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
      Hashtbl.add args.typed_classes ~key:typ ~data:typed_class;
      true

(* traverse the class tree in depth-first order from "Object" *)
let rec traverse_class_tree ~args root =
  let lazy_typecheck =
    lazy
      ((Hashtbl.mem T.reserved_classes root
       || Hashtbl.find args.untyped_classes root
          |> typecheck_class ~args ~typ:root)
      && Tree.find_out_edges args.inherit_tree root
         |> List.for_all ~f:(traverse_class_tree ~args))
  in
  (* allocate new identifier and method scopes when typechecking a new class *)
  Symtbl.enter_scope args.id_env
    ~cont:(lazy (Symtbl.enter_scope args.func_env ~cont:lazy_typecheck))

let validate ~args =
  (* "self" is always in scope and maps to "SELF_TYPE" *)
  Symtbl.add args.id_env ~key:T.self_var ~data:T.self_type |> ignore;
  traverse_class_tree ~args T.object_type
