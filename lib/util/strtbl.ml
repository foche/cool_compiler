(* strtbl.ml *)

open! MoreLabels

type 'a handle = Int.t

type 'a handle_module = (module Set.OrderedType with type t = 'a)

type ('a, 'b) t =
  ('b handle, 'a) Hashtbl.t * ('a, 'b handle) Hashtbl.t * 'b handle ref

let create n = (Hashtbl.create n, Hashtbl.create n, ref 0)

let add (handle_to_elem, elem_to_handle, i) x =
  match Hashtbl.find_opt elem_to_handle x with
  | Some j -> j
  | None ->
      let j = !i in
      Hashtbl.add handle_to_elem ~key:j ~data:x;
      Hashtbl.add elem_to_handle ~key:x ~data:j;
      i := j + 1;
      j

let find (handle_to_elem, _, _) = Hashtbl.find handle_to_elem

let length (_, _, i) = !i

let handle_module _ = (module Int : Set.OrderedType with type t = Int.t)
