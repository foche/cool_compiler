7(* typecheck.ml *)

open Parser
open Util
open Helpers
open Tables

let semant_verbose = ref false

let reserved_classes = init_hashtbl 16 [
    object_type, ();
    io_type, ();
    int_type, ();
    string_type, ();
    bool_type, ();
    self_type, ();
  ]

let inheritance_blocklist = init_hashtbl 8 [
    int_type, ();
    string_type, ();
    bool_type, ();
    self_type, ();
  ]

let basic_classes = [
    io_type;
    int_type;
    string_type;
    bool_type;
    self_type;
  ]

let basic_methods = [
    object_type, make_id "abort", object_type, [];
    object_type, make_id "type_name", string_type, [];
    object_type, make_id "copy", self_type, [];
    io_type, make_id "out_string", self_type, [make_id "x", string_type];
    io_type, make_id "out_int", self_type, [make_id "x", int_type];
    io_type, make_id "in_string", string_type, [];
    io_type, make_id "in_int", int_type, [];
    string_type, make_id "length", int_type, [];
    string_type, make_id "concat", string_type, [make_id "s", string_type];
    string_type, make_id "substr", string_type, [make_id "i", int_type; make_id "l", int_type];
  ]

let init_method_sigs _ =
  let tbl = Methodtbl.create 64 in
  List.iter (fun (clazz, method_id, return_type, formals) ->
    Methodtbl.add ~tbl ~clazz ~method_id ~return_type ~formals |> ignore)
    basic_methods;
  tbl

let init_inherit_graph _ =
  let graph = Hashtbl.create 32 in
  List.iter (fun cl -> Hashtbl.replace graph cl object_type) basic_classes;
  graph

let internal_typecheck (classes, line_number as program) =
  let id_env = Typeenv.create () in
  let func_env = Typeenv.create () in
  let graph = init_inherit_graph () in
  let sigs = init_method_sigs () in
  let typed_classes = Hashtbl.create 32 in
  let handle_to_class = Hashtbl.create 32 in
  let is_global_valid, tree_opt =
    Globalvalidator.validate {
        program;
        reserved_classes;
        inheritance_blocklist;
        handle_to_class;
        graph;
        sigs;
      } in

  let is_local_valid =
    is_global_valid &&
    Localvalidator.validate {
        ignored_classes = reserved_classes;
        id_env;
        func_env;
        graph = get_opt tree_opt;
        sigs;
        untyped_classes = handle_to_class;
        typed_classes;
      } in

  let replace_class (cl, _) =
    Hashtbl.find typed_classes cl.Ast.class_type in

  let replace_classes _ =
    let typed_classes = List.map replace_class classes in
    Some (typed_classes, line_number) in

  match is_local_valid with
  | false -> None
  | true -> replace_classes ()

let typecheck program =
  let program_opt = internal_typecheck program in
  (match program_opt, !semant_verbose with
  | None, _ -> Semantprint.print_typecheck_error ()
  | Some typed_program, true -> Astprint.print_ast typed_program
  | Some _, false -> ());
  program_opt
