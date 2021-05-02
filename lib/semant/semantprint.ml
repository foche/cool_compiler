(* semantprint.ml *)

module Abssyn = Parser.Abstractsyntax

let print_typecheck_error _ =
  Format.eprintf "@[<v>Compilation halted due to static semantic errors.@]@."

let print_error ~loc:(startpos, endpos) fmt =
  let line1 = startpos.Lexing.pos_lnum in
  let line2 = endpos.Lexing.pos_lnum in
  let offset1 = startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol + 1 in
  let offset2 = endpos.Lexing.pos_cnum - endpos.Lexing.pos_bol + 1 in
  Format.eprintf "@[<v>File %S: " startpos.Lexing.pos_fname;
  if line1 = line2 then
    Format.eprintf "line %d, characters %d-%d:@," line1 offset1 offset2
  else
    Format.eprintf "lines %d-%d, characters %d-%d:@," line1 line2 offset1
      offset2;
  "\027[31mError:\027[0m " ^^ fmt ^^ "@]@." |> Format.eprintf
