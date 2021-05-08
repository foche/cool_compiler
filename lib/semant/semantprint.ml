(* semantprint.ml *)

module Loc = Parser.Location

let print_typecheck_error _ =
  Format.eprintf "@[<v>Compilation halted due to static semantic errors.@]@."

let print_error ~loc fmt =
  Format.eprintf "@[<v>";
  Loc.print_location Format.err_formatter loc;
  "\027[31mError:\027[0m " ^^ fmt ^^ "@]@." |> Format.eprintf
