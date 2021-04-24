(* semantprint.ml *)

let print_typecheck_error _ =
  Format.eprintf "Compilation halted due to static semantic errors.@."

let print_error ~loc:(startpos, endpos) fmt =
  let line1 = startpos.Lexing.pos_lnum in
  let line2 = endpos.Lexing.pos_lnum in
  let offset1 = startpos.pos_cnum - startpos.pos_bol + 1 in
  let offset2 = endpos.pos_cnum - endpos.pos_bol + 1 in
  Format.eprintf "@[<v>File %S: " startpos.pos_fname;
  if line1 = line2 then
    Format.eprintf "line %d, characters %d-%d:@," line1 offset1 offset2
  else
    Format.eprintf "lines %d-%d, characters %d-%d:@," line1 line2 offset1
      offset2;
  Format.eprintf "\027[31mError:\027[0m ";
  fmt ^^ "@]@." |> Format.eprintf
