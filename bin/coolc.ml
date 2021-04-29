(*
  cc.ml

  Classroom Object-Oriented Language (COOL) compiler.
 *)

open StdLabels
open Parser
open Semant
open Translator
module Arch = Util.Architecture

(* let typechecker = *)

let usage_msg =
  Printf.sprintf "Usage: %s [-lpPS] [-o out_file] file [files]" Sys.argv.(0)

let input_files = ref []

let output_file = ref ""

let parse_input filename = input_files := filename :: !input_files

let spec_list =
  [
    ("-l", Arg.Set Coollexer.lexer_debug, "Enable lexer tracing");
    ( "-p",
      Arg.Unit (fun _ -> Parsing.set_trace true |> ignore),
      "Enable parser tracing" );
    ("-P", Arg.Set Parsedriver.parser_verbose, "Enable parser AST printing");
    ("-S", Arg.Set Typecheck.semant_verbose, "Enable typed AST printing");
    ("-o", Arg.Set_string output_file, "Set output file name");
  ]

let get_default_out_file file =
  let base =
    match String.rindex_opt file '.' with
    | None -> file
    | Some i -> String.sub file ~pos:0 ~len:i
  in
  Printf.sprintf "%s.s" base

let main _ =
  try
    let layout = Objectlayout.select Arch.Mips in
    let program_opt =
      Parsedriver.parse !input_files |> Option.map (Typecheck.typecheck layout)
    in
    let temp = Temp.create () |> Temp.fresh_temp in
    Temp.print Format.std_formatter temp;
    match program_opt with None -> exit 1 | Some program -> program |> ignore
  with Sys_error msg ->
    prerr_endline msg;
    exit 1

let _ =
  Arg.parse spec_list parse_input usage_msg;
  if List.compare_length_with !input_files ~len:0 = 0 then
    Arg.usage spec_list usage_msg
  else main ()
