(* typecheck.ml *)

open StdLabels
open MoreLabels
open Parser
open Util
module Abssyn = Abstractsyntax

let semant_verbose = ref false

let init_method_sigs class_count =
  let tbl = Methodtbl.create ((class_count * 8) - 1) in
  List.iter
    ~f:(fun (typ, method_id, ret_typ, formals) ->
      Methodtbl.add tbl ~typ ~method_id ~ret_typ ~formals |> ignore)
    Tables.basic_methods;
  tbl

let init_parents class_count =
  let parents = Hashtbl.create ((class_count * 2) - 1) in
  List.iter
    ~f:(fun cl -> Hashtbl.replace parents ~key:cl ~data:Tables.object_type)
    Tables.basic_classes;
  parents

let internal_typecheck program =
  let id_count = Strtbl.length Tables.id_tbl in
  let class_count = List.length program.Abssyn.elem in
  let id_env = Symtbl.create ((id_count * 2) - 1) in
  let func_env = Symtbl.create ((id_count * 2) - 1) in
  let parents = init_parents class_count in
  let sigs = init_method_sigs class_count in
  let typed_classes = Hashtbl.create ((class_count * 2) - 1) in
  let handle_to_class = Hashtbl.create ((class_count * 2) - 1) in
  let is_global_valid, tree_opt =
    Globalvalidator.validate ~args:{ program; handle_to_class; parents; sigs }
  in
  let is_valid =
    is_global_valid
    && Localvalidator.validate
         ~args:
           {
             id_env;
             func_env;
             inherit_tree = Option.get tree_opt;
             sigs;
             untyped_classes = handle_to_class;
             typed_classes;
           }
  in
  let replace_class (cl : Abssyn.class_node) =
    Hashtbl.find typed_classes cl.elem.cl_typ
  in
  if is_valid then
    let typed_classes = List.map ~f:replace_class program.elem in
    Some { program with Abssyn.elem = typed_classes }
  else None

let typecheck program =
  let program_opt = internal_typecheck program in
  (match (program_opt, !semant_verbose) with
  | None, _ -> Semantprint.print_typecheck_error ()
  | Some typed_program, true -> Astprint.print_ast typed_program
  | Some _, false -> ());
  program_opt
