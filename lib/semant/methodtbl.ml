(* methodtbl.ml *)

open MoreLabels
open Parser
open Util

type method_sig = {
  ret_typ : Tables.typ_sym;
  formals : Abstractsyntax.var_decl list;
  impl_class : Tables.typ_sym;
  label : Tables.id_sym;
}

type t = (Tables.typ_sym * Tables.id_sym, method_sig) Hashtbl.t

let create = Hashtbl.create ~random:false

let add sigs ~typ ~method_id ~ret_typ ~formals =
  match Hashtbl.find_opt sigs (typ, method_id) with
  | Some _ -> false
  | None ->
      Hashtbl.add sigs ~key:(typ, method_id)
        ~data:
          {
            ret_typ;
            formals;
            impl_class = typ;
            label = Tables.method_label typ method_id;
          };
      true

let find_opt sigs ~inherit_tree ~typ ~method_id =
  let rec aux ~typ ~cont =
    match Hashtbl.find_opt sigs (typ, method_id) with
    | Some _ as sig_opt -> cont sig_opt
    | None -> (
        match Tree.find_parent_opt inherit_tree typ with
        | None -> cont None
        | Some parent ->
            aux ~typ:parent ~cont:(fun sig_opt ->
                Option.iter
                  (fun method_sig ->
                    (* perform lazy path compression like in union-find *)
                    Hashtbl.add sigs ~key:(typ, method_id) ~data:method_sig)
                  sig_opt;
                cont sig_opt))
  in
  aux ~typ ~cont:Fun.id
