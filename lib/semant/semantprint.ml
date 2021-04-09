(* semantprint.ml *)

open Util

let print_typecheck_error _ =
  prerr_endline "Compilation halted due to static semantic errors."

let print_location filename line_number =
  Printf.eprintf "%a:%d: " Tables.print_str filename line_number
