(* localvalidator.ml *)

open Parser
open Ast
open Util
open Helpers
open Tables

type validator_args = {
    ignored_classes : (type_sym, unit) Hashtbl.t;
    id_env : (id_sym, type_sym) Symtbl.t;
    func_env : (id_sym, mthd) Symtbl.t;
    graph : type_sym Tree.t;
    sigs : Methodtbl.t;
    untyped_classes : (type_sym, class_node) Hashtbl.t;
    typed_classes : (type_sym, class_node) Hashtbl.t;
  }

let typecheck_field args cl line_number id typ (init, _ as init_node) =
  match init.typ_expr with
  | NoExpr -> Some (Field (id, typ, init_node), line_number)
  | _ ->
    let typed_init =
      Exprchecker.typecheck
        ~ctx:{
          id_env = args.id_env;
          sigs = args.sigs;
          graph = args.graph;
          cl;
          filename = cl.class_filename;
        }
        ~exp_node:init_node
    in
      match get_exp_type typed_init with
      | None -> None
      | Some init_typ ->
        match is_subtype args.graph cl init_typ typ with
        | true -> Some (Field (id, typ, typed_init), line_number)
        | false ->
          Semantprint.print_location cl.class_filename line_number;
          Printf.eprintf
            "Inferred type %a of initialization of attribute %a does not conform to declared type %a.\n"
            print_type init_typ
            print_id id
            print_type typ;
          None

let typecheck_method args cl mthd line_number =
  let add_formal ((id, typ), line_number') =
    match Symtbl.add args.id_env id typ |> fst with
    | false -> true
    | true ->
      Semantprint.print_location cl.class_filename line_number';
      Printf.eprintf
        "Formal parameter %a is multiply defined.\n"
        print_id id;
      false in

  let lazy_typecheck =
    lazy (
      match List.for_all add_formal mthd.method_args with
      | false -> mthd.method_body
      | true ->
        Exprchecker.typecheck
          ~ctx:{
            id_env = args.id_env;
            sigs = args.sigs;
            graph = args.graph;
            cl;
            filename = cl.class_filename;
          }
          ~exp_node:mthd.method_body) in

  let typed_body = Symtbl.enter_scope args.id_env lazy_typecheck in
  match get_exp_type typed_body with
  | None -> None
  | Some typ ->
    match is_subtype args.graph cl typ mthd.method_ret_type with
    | true -> Some (Method {mthd with method_body = typed_body}, line_number)
    | false ->
      Semantprint.print_location cl.class_filename line_number;
      Printf.eprintf
        "Inferred return type %a of method %a does not conform to declared return type %a.\n"
        print_type typ
        print_id mthd.method_id
        print_type mthd.method_ret_type;
      None

let typecheck_feature args cl (feat, line_number) =
  match feat with
  | Field (id, typ, init) -> typecheck_field args cl line_number id typ init
  | Method mthd -> typecheck_method args cl mthd line_number

let validate_formal_types filename method_id ((_, typ), line_number) ((_, parent_typ), _) =
  match typ = parent_typ with
  | true -> true
  | false ->
    Semantprint.print_location filename line_number;
    Printf.eprintf
      "In redefined method %a, parameter type %a is different from original type %a.\n"
      print_id method_id
      print_type typ
      print_type parent_typ;
    false

let validate_method_sig filename line_number mthd parent_method =
  let param_counts_match =
    List.compare_lengths
      mthd.method_args
      parent_method.method_args = 0 in

  let return_types_match = mthd.method_ret_type = parent_method.method_ret_type in
  let formal_types_match =
    param_counts_match &&
    List.for_all2
      (validate_formal_types filename mthd.method_id)
      mthd.method_args
      parent_method.method_args in

  (match param_counts_match with
  | true -> ()
  | false ->
    Semantprint.print_location filename line_number;
    Printf.eprintf
      "Incompatible number of formal parameters in redefined method %a.\n"
      print_id mthd.method_id);

  (match return_types_match with
  | true -> ()
  | false ->
    Semantprint.print_location filename line_number;
    Printf.eprintf
      "In redefined method %a, return type %a is different from original return type %a.\n"
      print_id mthd.method_id
      print_type mthd.method_ret_type
      print_type parent_method.method_ret_type);
  param_counts_match && return_types_match && formal_types_match

let extract_method func_env filename line_number mthd =
  let parent_method_opt = Symtbl.find_opt func_env mthd.method_id in
  let _, is_overridden = Symtbl.add func_env mthd.method_id mthd in
  match is_overridden with
  | false -> true
  | true -> get_opt parent_method_opt |> validate_method_sig filename line_number mthd

let extract_field id_env cl line_number id typ =
  let is_duplicate, is_shadowed = Symtbl.add id_env id typ in

  (match is_duplicate with
  | false -> ()
  | true ->
    Semantprint.print_location cl.class_filename line_number;
    Printf.eprintf
      "Attribute %a is multiply defined in class %a.\n"
      print_id id
      print_type cl.class_type);

  (match is_shadowed with
  | false -> ()
  | true ->
    Semantprint.print_location cl.class_filename line_number;
    Printf.eprintf
      "Attribute %a is an attribute of an inherited class.\n"
      print_id id);

  not is_duplicate && not is_shadowed

let extract_feature id_env func_env cl (feat, line_number) =
  match feat with
  | Method mthd -> extract_method func_env cl.class_filename line_number mthd
  | Field (id, typ, _) -> extract_field id_env cl line_number id typ


let typecheck_class args typ (cl, line_number) =
  let valid_field_decl =
    List.for_all
      (extract_feature args.id_env args.func_env cl)
      cl.class_features in

  match valid_field_decl with
  | false -> false
  | true ->
    let typed_feature_opt =
      List.rev_map
        (typecheck_feature args cl)
        cl.class_features |>
      flatten_opt_list in

    match typed_feature_opt with
    | None -> false
    | Some typed_features ->
      let typed_class = ({cl with
          class_features = typed_features;
        },
        line_number) in
      Hashtbl.replace args.typed_classes typ typed_class;
      true

let rec traverse_class_tree args root =
  Symtbl.enter_scope args.id_env (lazy (
    Symtbl.enter_scope args.func_env (lazy (
      (Hashtbl.mem args.ignored_classes root ||
        Hashtbl.find args.untyped_classes root |> typecheck_class args root) &&
      Tree.find_out_edges args.graph root |>
      List.for_all (traverse_class_tree args)))))

let validate args =
  Symtbl.enter_scope args.id_env (lazy (
    Symtbl.add args.id_env self_var self_type |> ignore;
    traverse_class_tree args object_type))
