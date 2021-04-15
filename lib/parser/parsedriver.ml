(* parsedriver.ml *)

open Util
open Helpers

let parser_verbose = ref false

let run_on_file f acc filename =
  let file = open_in filename in
  let acc' = try f acc filename file with ex -> close_in file ; raise ex in
  close_in file ; acc'

let parse_single (acc, empty_count) filename file =
  let buf = Lexing.from_channel file in
  buf.lex_curr_p <- {buf.lex_curr_p with pos_fname= filename} ;
  buf.lex_start_p <- {buf.lex_start_p with pos_fname= filename} ;
  Coollexer.print_filename filename ;
  let cls_opt, is_empty =
    try Coolparser.parse Coollexer.get_token buf with Parsing.Parse_error ->
      (None, false)
  in
  let new_count = empty_count + if is_empty then 1 else 0 in
  (cls_opt :: acc, new_count)

let internal_parse filenames =
  let cls_opts, empty_count =
    List.fold_left (run_on_file parse_single) ([], 0) filenames
  in
  let cls =
    List.rev_map (get_opt ~default:(Some [])) cls_opts |> List.concat
  in
  let all_empty = List.compare_length_with filenames empty_count = 0 in
  let empty_cls = List.compare_length_with cls 0 = 0 in
  match (all_empty, empty_cls) with
  | true, _ ->
      List.hd filenames |> Astprint.print_eof_error ;
      None
  | false, true -> None
  | false, false ->
      let _, line_number = List.hd cls in
      Some (cls, line_number)

let parse filenames =
  let program_opt = internal_parse filenames in
  ( match (program_opt, !parser_verbose) with
  | None, _ -> Astprint.print_syntax_error ()
  | Some program, true -> Astprint.print_ast program
  | Some _, false -> () ) ;
  program_opt
