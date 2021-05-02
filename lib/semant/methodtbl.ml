(* methodtbl.ml *)

open! MoreLabels
module Abssyn = Parser.Abstractsyntax
module Tbls = Util.Tables
module Tree = Util.Tree

type method_sig = {
  method_ret_typ : Tbls.typ_sym;
  formals : Abssyn.var_decl List.t;
  impl_class : Tbls.typ_sym;
  label : Tbls.id_sym;
}

type t = (Tbls.typ_sym * Tbls.id_sym, method_sig) Hashtbl.t

let create = Hashtbl.create ~random:false

let add sigs ~cl_typ ~method_id ~method_ret_typ ~formals =
  match Hashtbl.find_opt sigs (cl_typ, method_id) with
  | Some _ -> false
  | None ->
      Hashtbl.add sigs ~key:(cl_typ, method_id)
        ~data:
          {
            method_ret_typ;
            formals;
            impl_class = cl_typ;
            label = Tbls.method_label cl_typ method_id;
          };
      true

let find_opt sigs ~inherit_tree ~cl_typ ~method_id =
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
  aux ~typ:cl_typ ~cont:Fun.id
