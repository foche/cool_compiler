(* strtbl.ml *)

type handle = int

type 'a t = (handle, 'a) Hashtbl.t * ('a, handle) Hashtbl.t * handle ref

let create n =
  Hashtbl.create n, Hashtbl.create n, ref 0

let add (handle_to_elem, elem_to_handle, i) x =
  match Hashtbl.find_opt elem_to_handle x with
  | Some j -> j
  | None ->
    let j = !i in
    Hashtbl.replace handle_to_elem j x;
    Hashtbl.replace elem_to_handle x j;
    i := j + 1;
    j

let remove (handle_to_elem, elem_to_handle, _) handle =
  let x = Hashtbl.find handle_to_elem handle in
  Hashtbl.remove handle_to_elem handle;
  Hashtbl.remove elem_to_handle x

let find (handle_to_elem, _, _) =
  Hashtbl.find handle_to_elem
