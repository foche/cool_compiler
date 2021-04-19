(* semantprint.ml *)

[@@@coverage exclude_file]

let print_typecheck_error _ =
  prerr_endline "Compilation halted due to static semantic errors."

let print_location (startpos, endpos) =
  Printf.eprintf "File %S: " startpos.Lexing.pos_fname;
  let line1 = startpos.pos_lnum in
  let line2 = endpos.Lexing.pos_lnum in
  let offset1 = startpos.pos_cnum - startpos.pos_bol in
  let offset2 = endpos.pos_cnum - endpos.pos_bol in
  if line1 = line2 then
    Printf.eprintf "line %d, characters %d-%d:\n" line1 offset1 offset2
  else
    Printf.eprintf "line %d, character %d to line %d, character %d\n" line1
      offset1 line2 offset2
