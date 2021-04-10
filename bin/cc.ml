(*
 * cc.ml
 *
 * Classroom Object-Oriented Language (COOL) compiler.
 *)

open Parser
open Semant
open Util
open Helpers

let usage_msg = Printf.sprintf "Usage: %s [-lpPS] [-o out_file] file [files]" Sys.argv.(0)
let input_files = ref []
let output_file = ref ""
let parse_input filename =
  input_files := filename :: !input_files
let out_file_extension = "s"

let spec_list = [
    "-l", Arg.Set Coollexer.lexer_debug, "Enable lexer tracing";
    "-p", Arg.Unit (fun _ -> Parsing.set_trace true |> ignore), "Enable parser tracing";
    "-P", Arg.Set Parsedriver.parser_verbose, "Enable parser AST printing";
    "-S", Arg.Set Typecheck.semant_verbose, "Enable typed AST printing";
    "-o", Arg.Set_string output_file, "Set output file name";
  ]

let get_default_out_file file =
  let splits = String.split_on_char '.' file in
  match List.compare_length_with splits 1 with
  | 0 -> List.hd splits ^ "." ^ out_file_extension
  | _ ->
    List.rev splits |>
    List.tl |>
    List.cons out_file_extension |>
    List.rev |>
    String.concat "."

 let main _ =
  try
    let program_opt = Parsedriver.parse !input_files in
    let _ = map_opt ~f:Typecheck.typecheck program_opt in
    let actual_output_file =
      match !output_file with
      | "" -> List.rev !input_files |> List.hd |> get_default_out_file
      | out_file -> out_file in
    let out_file = open_out actual_output_file in
    close_out out_file
  with
    | Sys_error msg -> prerr_endline msg

let _ =
  Arg.parse spec_list parse_input usage_msg;
  match List.compare_length_with !input_files 0 with
  | 0 -> Arg.usage spec_list usage_msg
  | _ -> main ()
