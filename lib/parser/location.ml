type t = { startpos : Lexing.position; endpos : Lexing.position }

type raw_pos = Lexing.position * Lexing.position

let create (startpos, endpos) = { startpos; endpos }

let print_location ppf { startpos; endpos } =
  let line1 = startpos.Lexing.pos_lnum in
  let line2 = endpos.Lexing.pos_lnum in
  let offset1 = startpos.Lexing.pos_cnum - startpos.Lexing.pos_bol + 1 in
  let offset2 = endpos.Lexing.pos_cnum - endpos.Lexing.pos_bol + 1 in
  Format.fprintf ppf "File %S: " startpos.Lexing.pos_fname;
  if line1 = line2 then
    Format.fprintf ppf "line %d, characters %d-%d:@," line1 offset1 offset2
  else
    Format.fprintf ppf "lines %d-%d, characters %d-%d:@," line1 line2 offset1
      offset2

let start_line_num { startpos = { Lexing.pos_lnum; _ }; _ } = pos_lnum

let filename { startpos = { Lexing.pos_fname; _ }; _ } = pos_fname
