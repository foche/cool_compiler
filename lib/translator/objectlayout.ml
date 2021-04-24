(* objlayout.ml *)

open Util

module type Layout = sig
  type t

  type access

  val create : typ:Tables.typ_sym -> t

  val copy : t -> typ:Tables.typ_sym -> t

  val alloc_field : t -> Tables.id_sym -> size:int -> access

  val alloc_method : t -> Tables.id_sym -> label:Labeler.label -> access

  val access_field : t -> Tables.id_sym -> access
end
