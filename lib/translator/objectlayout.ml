(* objectlayout.ml *)

open Util
module Arch = Architecture
module Tbls = Util.Tables

module type S = sig
  type t

  type access

  val create : typ:Tbls.typ_sym -> t

  val copy : t -> typ:Tbls.typ_sym -> t

  val alloc_field : t -> Tbls.id_sym -> size:int -> access

  val alloc_method : t -> Tbls.id_sym -> label:Labeler.label -> access

  val access_field : t -> Tbls.id_sym -> access
end

let select = function
  | Arch.Mips -> (module Mipsobjlayout : S)
  | Arch.X86 -> (module X86objlayout : S)
