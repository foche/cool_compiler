(* objlayout.ml *)

open Util

module type ObjLayout = sig
  type t

  type access

  val create : typ:Tables.type_sym -> t

  val copy : t -> typ:Tables.type_sym -> t

  val alloc_field : t -> Tables.id_sym -> size:int -> access

  val alloc_method : t -> Tables.id_sym -> label:Temp.label -> access

  val access_field : t -> Tables.id_sym -> access
end
