(* parsedriver.ml *)

open StdLabels
module Abssyn = Abstractsyntax

let parser_verbose = ref false

let run_on_file ~f acc filename =
  let file = open_in filename in
  let acc' =
    try f acc filename file
    with ex ->
      close_in file;
      raise ex
  in
  close_in file;
  acc'

let parse_single (acc, empty_count) filename file =
  let buf = Lexing.from_channel file in
  buf.lex_curr_p <- { buf.lex_curr_p with pos_fname = filename };
  buf.lex_start_p <- { buf.lex_start_p with pos_fname = filename };
  Coollexer.print_filename filename;
  let cls_opt, is_empty =
    try Coolparser.parse Coollexer.get_token buf
    with Coolparser.Error -> (None, false)
  in
  let new_count = empty_count + if is_empty then 1 else 0 in
  (cls_opt :: acc, new_count)

let internal_parse filenames =
  let programs, empty_count =
    List.fold_left ~f:(run_on_file ~f:parse_single) ~init:([], 0) filenames
  in
  let cls =
    List.rev_map ~f:(Option.value ~default:[]) programs |> List.concat
  in
  let all_empty = List.compare_length_with filenames ~len:empty_count = 0 in
  if all_empty then (
    List.hd filenames |> Astprint.print_eof_error;
    None)
  else
    match cls with
    | [] -> None
    | cl :: _ -> Some { Abssyn.elem = cls; loc = cl.Abssyn.loc }

let parse filenames =
  let program_opt = internal_parse filenames in
  (match program_opt with
  | None -> Astprint.print_syntax_error ()
  | Some program -> if !parser_verbose then Astprint.print_ast program);
  program_opt
