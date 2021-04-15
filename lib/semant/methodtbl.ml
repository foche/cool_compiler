(* methodtbl.ml *)

open Parser
open Util
open Helpers

type method_sig =
  { return_type: Tables.type_sym
  ; formals: Ast.formal list
  ; impl_class: Tables.type_sym
  ; label: Tables.id_sym }

type t = (Tables.type_sym * Tables.id_sym, method_sig) Hashtbl.t

let create = Hashtbl.create ~random:false

let add ~tbl ~clazz ~method_id ~return_type ~formals =
  match Hashtbl.find_opt tbl (clazz, method_id) with
  | Some _ -> false
  | None ->
      Hashtbl.replace tbl (clazz, method_id)
        { return_type
        ; formals
        ; impl_class= clazz
        ; label= Tables.method_label clazz method_id } ;
      true

let find_opt ~tbl ~graph ~clazz ~method_id =
  let rec aux cl cont =
    match Hashtbl.find_opt tbl (cl, method_id) with
    | Some _ as sig_opt -> cont sig_opt
    | None -> (
      match Tree.find_parent_opt graph cl with
      | None -> cont None
      | Some parent ->
          aux parent (fun sig_opt ->
              map_opt
                ~f:(fun method_sig ->
                  Hashtbl.replace tbl (cl, method_id) method_sig ;
                  None )
                sig_opt
              |> ignore ;
              cont sig_opt ) )
  in
  aux clazz (fun sig_opt -> sig_opt)

let iter ~f ~tbl =
  Hashtbl.iter
    (fun (clazz, method_id) mthd_sig -> f clazz method_id mthd_sig)
    tbl

let for_all ~f ~tbl =
  Hashtbl.fold
    (fun (clazz, method_id) mthd_sig acc -> acc && f clazz method_id mthd_sig)
    tbl true
