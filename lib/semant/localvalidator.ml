(* localvalidator.ml *)

open! StdLabels
open! MoreLabels
module Abssyn = Parser.Abstractsyntax
module Opts = Util.Optutil
module Symtbl = Util.Symtbl
module Tbls = Util.Tables
module Tree = Util.Tree

type validator_args = {
  id_env : (Tbls.id_sym, Tbls.typ_sym) Symtbl.t;
  func_env : (Tbls.id_sym, Tbls.typ_sym * Abssyn.method_def) Symtbl.t;
  inherit_tree : Tbls.typ_sym Tree.t;
  sigs : Methodtbl.t;
  untyped_classes : (Tbls.typ_sym, Abssyn.class_node) Hashtbl.t;
  typed_classes : (Tbls.typ_sym, Abssyn.class_node) Hashtbl.t;
}

let check_field_init ~args ~feature ~cl_typ ~field_def ~typed_init =
  let { Abssyn.expr_typ; expr_loc; _ } = typed_init in
  match expr_typ with
  | None -> None
  | Some init_typ ->
      let field_id, field_typ = field_def.Abssyn.field_var.Abssyn.elem in
      let is_valid_init_type =
        Types.is_subtype args.inherit_tree ~cl_typ ~sub_typ:init_typ
          ~super_typ:field_typ
      in
      if is_valid_init_type then
        Some
          {
            feature with
            Abssyn.elem =
              Abssyn.Field { field_def with Abssyn.field_init = typed_init };
          }
      else (
        Semantprint.print_error ~loc:expr_loc
          "Inferred type %a of initialization of attribute %a does not conform \
           to declared type %a."
          Tbls.print_type init_typ Tbls.print_id field_id Tbls.print_type
          field_typ;
        None)

let typecheck_field ~args ~cl_typ ~feature ~field_def =
  let { Abssyn.field_var = { Abssyn.elem = _, field_typ; _ }; field_init } =
    field_def
  in
  let typed_init =
    Exprchecker.typecheck ~super_typ:field_typ
      ~ctx:
        {
          Exprchecker.id_env = args.id_env;
          sigs = args.sigs;
          inherit_tree = args.inherit_tree;
          cl_typ;
        }
      field_init
  in
  check_field_init ~args ~feature ~cl_typ ~field_def ~typed_init

let check_method_body ~inherit_tree ~cl_typ ~feature ~method_def ~typed_body =
  let { Abssyn.expr_typ; expr_loc; _ } = typed_body in
  match expr_typ with
  | None -> None
  | Some body_typ ->
      let { Abssyn.method_ret_typ; method_id; _ } = method_def in
      let is_valid_body_type =
        Types.is_subtype inherit_tree ~cl_typ ~sub_typ:body_typ
          ~super_typ:method_ret_typ
      in
      if is_valid_body_type then
        Some
          {
            feature with
            Abssyn.elem =
              Abssyn.Method { method_def with Abssyn.method_body = typed_body };
          }
      else (
        Semantprint.print_error ~loc:expr_loc
          "Inferred return type %a of method %a does not conform to declared \
           return type %a."
          Tbls.print_type body_typ Tbls.print_id method_id Tbls.print_type
          method_ret_typ;
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
                  "Formal parameter %a is multiply defined." Tbls.print_id id))
           true
       in
       let { Abssyn.method_formals; method_body; _ } = method_def in
       if List.for_all ~f:add_formal method_formals then
         Exprchecker.typecheck
           ~ctx:
             {
               Exprchecker.id_env = args.id_env;
               sigs = args.sigs;
               inherit_tree = args.inherit_tree;
               cl_typ;
             }
           method_body
       else method_body)
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
  let { Abssyn.elem = _, typ; loc } = formal in
  let { Abssyn.elem = _, parent_typ; _ } = parent_formal in
  Validator.bind
    ~checker:(lazy (typ = parent_typ))
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc
           "In redefined method %a, parameter type %a is different from \
            original type %a."
           Tbls.print_id method_id Tbls.print_type typ Tbls.print_type
           parent_typ))
    true

let validate_overridden_method ~method_def ~loc ~overridden_method =
  let { Abssyn.method_id; method_ret_typ; method_formals; _ } = method_def in
  let {
    Abssyn.method_ret_typ = parent_ret_typ;
    method_formals = parent_formals;
    _;
  } =
    overridden_method
  in
  Validator.bind
    ~checker:(lazy (List.compare_lengths method_formals parent_formals = 0))
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc
           "Incompatible number of formal parameters in redefined method %a."
           Tbls.print_id method_id))
    true
  |> Validator.bind
       ~checker:(lazy (method_ret_typ = parent_ret_typ))
       ~err_fun:
         (lazy
           (Semantprint.print_error ~loc
              "In redefined method %a, return type %a is different from \
               original return type %a."
              Tbls.print_id method_id Tbls.print_type method_ret_typ
              Tbls.print_type parent_ret_typ))
  && List.for_all2
       ~f:(validate_formal_types ~method_id)
       method_formals parent_formals

let extract_method ~args ~cl_typ ~method_def ~loc =
  let method_id = method_def.Abssyn.method_id in
  let overridden_method_opt = Symtbl.find_opt args.func_env method_id in
  Symtbl.add args.func_env ~key:method_id ~data:(cl_typ, method_def) |> ignore;
  match overridden_method_opt with
  | None -> true
  | Some (parent_typ, overridden_method) ->
      Methodtbl.set_is_final args.sigs ~cl_typ:parent_typ ~method_id false;
      validate_overridden_method ~method_def ~loc ~overridden_method

let extract_field ~id_env ~cl_typ
    ~field_var:{ Abssyn.elem = field_id, field_typ; loc } =
  let is_duplicate, is_shadowed =
    Symtbl.add id_env ~key:field_id ~data:field_typ
  in
  if is_duplicate then
    Semantprint.print_error ~loc "Attribute %a is multiply defined in class %a."
      Tbls.print_id field_id Tbls.print_type cl_typ;
  if is_shadowed then
    Semantprint.print_error ~loc
      "Attribute %a is an attribute of an inherited class." Tbls.print_id
      field_id;
  (not is_duplicate) && not is_shadowed

let extract_feature ~args ~cl_typ feature =
  let { Abssyn.elem; loc } = feature in
  match elem with
  | Abssyn.Method method_def -> extract_method ~args ~cl_typ ~method_def ~loc
  | Abssyn.Field { Abssyn.field_var; _ } ->
      extract_field ~id_env:args.id_env ~cl_typ ~field_var

let typecheck_class ~args ~typ cl =
  let { Abssyn.elem = { Abssyn.cl_typ; cl_features; _ } as elem; _ } = cl in
  List.for_all ~f:(extract_feature ~args ~cl_typ) cl_features
  &&
  let typed_feature_opt =
    List.rev_map ~f:(typecheck_feature ~args ~cl_typ) cl_features
    |> Opts.flatten_opt_list
  in
  match typed_feature_opt with
  | None -> false
  | Some typed_features ->
      let typed_class =
        {
          cl with
          Abssyn.elem = { elem with Abssyn.cl_features = typed_features };
        }
      in
      Hashtbl.add args.typed_classes ~key:typ ~data:typed_class;
      true

(* traverse the class tree in depth-first order from "Object" *)
let rec traverse_class_tree ~args root =
  let lazy_typecheck =
    lazy
      ((Hashtbl.mem Tbls.reserved_classes root
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
  Symtbl.add args.id_env ~key:Tbls.self_var ~data:Tbls.self_type |> ignore;
  traverse_class_tree ~args Tbls.object_type
