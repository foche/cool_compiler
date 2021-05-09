(* localvalidator.ml *)

open! StdLabels
open! MoreLabels
module Abssyn = Parser.Abstractsyntax
module Ast = Parser.Ast
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

let check_field_init ~args ~cl_typ ~loc ~field_def typed_init =
  let { Abssyn.expr_typ; expr_loc; _ } = typed_init in
  match expr_typ with
  | None -> None
  | Some init_typ ->
      let field_id, field_typ = field_def.Abssyn.field_var.Abssyn.elem in
      Types.is_subtype args.inherit_tree ~cl_typ ~sub_typ:init_typ
        ~super_typ:field_typ
      |> Validator.fold
           ~err_fun:
             (lazy
               (Semantprint.print_error ~loc:expr_loc
                  "Inferred type %a of initialization of attribute %a does not \
                   conform to declared type %a."
                  Tbls.print_type init_typ Tbls.print_id field_id
                  Tbls.print_type field_typ))
           ~fail:None
           ~success:
             (lazy
               (Some
                  {
                    Abssyn.elem =
                      Abssyn.Field
                        { field_def with Abssyn.field_init = typed_init };
                    loc;
                  }))

let typecheck_field ~args ~cl_typ ~loc field_def =
  let { Abssyn.field_var = { Abssyn.elem = _, field_typ; _ }; field_init } =
    field_def
  in
  Exprchecker.typecheck ~super_typ:field_typ ~is_tail_pos:false
    ~ctx:
      {
        Exprchecker.id_env = args.id_env;
        sigs = args.sigs;
        inherit_tree = args.inherit_tree;
        cl_typ;
      }
    field_init
  |> check_field_init ~args ~cl_typ ~loc ~field_def

let check_method_body ~inherit_tree ~cl_typ ~loc ~method_def typed_body =
  let { Abssyn.expr_typ; expr_loc; _ } = typed_body in
  match expr_typ with
  | None -> None
  | Some body_typ ->
      let { Abssyn.method_ret_typ; method_id; _ } = method_def in
      Types.is_subtype inherit_tree ~cl_typ ~sub_typ:body_typ
        ~super_typ:method_ret_typ
      |> Validator.fold
           ~err_fun:
             (lazy
               (Semantprint.print_error ~loc:expr_loc
                  "Inferred return type %a of method %a does not conform to \
                   declared return type %a."
                  Tbls.print_type body_typ Tbls.print_id method_id
                  Tbls.print_type method_ret_typ))
           ~fail:None
           ~success:
             (lazy
               (Some
                  {
                    Abssyn.elem =
                      Abssyn.Method
                        { method_def with Abssyn.method_body = typed_body };
                    loc;
                  }))

let typecheck_method ~args ~cl_typ ~loc method_def =
  let add_formal { Abssyn.elem = id, typ; loc } =
    Validator.map
      ~pred:(Symtbl.add args.id_env ~key:id ~data:typ |> fst |> not)
      ~err_fun:
        (lazy
          (Semantprint.print_error ~loc
             "Formal parameter %a is multiply defined." Tbls.print_id id))
      true
  in
  let { Abssyn.method_formals; method_body; _ } = method_def in
  let cont =
    lazy
      (if List.for_all ~f:add_formal method_formals then
       Exprchecker.typecheck ~is_tail_pos:true
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
  Symtbl.enter_scope args.id_env ~cont
  |> check_method_body ~inherit_tree:args.inherit_tree ~cl_typ ~loc ~method_def

let validate_formal_types ~method_id formal parent_formal =
  let { Abssyn.elem = _, typ; loc } = formal in
  let { Abssyn.elem = _, parent_typ; _ } = parent_formal in
  Validator.map ~pred:(typ = parent_typ)
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
  Validator.map
    ~pred:(List.compare_lengths method_formals parent_formals = 0)
    ~err_fun:
      (lazy
        (Semantprint.print_error ~loc
           "Incompatible number of formal parameters in redefined method %a."
           Tbls.print_id method_id))
    true
  |> Validator.map
       ~pred:(method_ret_typ = parent_ret_typ)
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

let extract_method ~args ~cl_typ ~loc method_def =
  let method_id = method_def.Abssyn.method_id in
  let overridden_method_opt = Symtbl.find_opt args.func_env method_id in
  Symtbl.add args.func_env ~key:method_id ~data:(cl_typ, method_def) |> ignore;
  match overridden_method_opt with
  | None -> true
  | Some (parent_typ, overridden_method) ->
      Methodtbl.set_is_final args.sigs ~cl_typ:parent_typ ~method_id false;
      validate_overridden_method ~method_def ~loc ~overridden_method

let extract_field ~id_env ~cl_typ ~loc:_
    { Abssyn.field_var = { Abssyn.elem = field_id, field_typ; loc }; _ } =
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

let typecheck_features ~args ~cl_typ cl =
  Ast.rev_map_features
    ~method_f:(typecheck_method ~args ~cl_typ)
    ~field_f:(typecheck_field ~args ~cl_typ)
    cl
  |> Opts.flatten_opt_list
  |> Option.fold ~none:false ~some:(fun typed_features ->
         Hashtbl.add args.typed_classes ~key:cl_typ
           ~data:(Ast.replace_features ~cl typed_features);
         true)

let typecheck_class ~args ~cl_typ =
  if Hashtbl.mem Tbls.reserved_classes cl_typ then true
  else
    let cl = Hashtbl.find args.untyped_classes cl_typ in
    Ast.for_all_features
      ~method_f:(extract_method ~args ~cl_typ)
      ~field_f:(extract_field ~id_env:args.id_env ~cl_typ)
      cl
    && typecheck_features ~args ~cl_typ cl

(* traverse the class tree in depth-first order from "Object" *)
let rec traverse_class_tree ~args
    (module MethodSet : Set.S with type elt = Tbls.id_sym) (is_valid, set)
    cl_typ =
  let lazy_typecheck =
    lazy
      (let is_valid_class = typecheck_class ~args ~cl_typ in
       Tree.find_out_edges args.inherit_tree cl_typ
       |> List.fold_left
            ~f:(traverse_class_tree ~args (module MethodSet))
            ~init:(is_valid && is_valid_class, set))
  in
  (* allocate new identifier and method scopes when typechecking a new class *)
  Symtbl.enter_scope args.id_env
    ~cont:(lazy (Symtbl.enter_scope args.func_env ~cont:lazy_typecheck))

let validate ~args =
  (* "self" is always in scope and maps to "SELF_TYPE" *)
  Symtbl.add args.id_env ~key:Tbls.self_var ~data:Tbls.self_type |> ignore;
  let (module H) = Tbls.id_module in
  let module MethodSet = Set.Make (H) in
  traverse_class_tree ~args
    (module MethodSet)
    (true, MethodSet.empty) Tbls.object_type
  |> fst
