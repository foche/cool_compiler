(* varversion.ml *)

open Util

type t = (Tables.id_sym, int) Hashtbl.t

let create = Hashtbl.create ~random:false

let find = Hashtbl.find

let add var_versions id =
  match Hashtbl.find_opt var_versions id with
  | Some ver ->
      Hashtbl.replace var_versions id (ver + 1);
      ver + 1
  | None ->
      Hashtbl.add var_versions id 0;
      0
