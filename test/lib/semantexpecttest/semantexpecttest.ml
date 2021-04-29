(* semantexpecttest.ml *)

open Parser
open Semant
open Translator

let _ =
  Typecheck.semant_verbose := true;
  Parsedriver.parse [ Sys.argv.(1) ]
  |> Option.get
  |> Typecheck.typecheck (module Mipsobjlayout)
