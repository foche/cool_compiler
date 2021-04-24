(* temp.ml *)

type t = int ref

type temp = int

let create _ = ref 0

let fresh_temp temp =
  let i = !temp in
  temp := i + 1;
  i

let print ppf = Format.fprintf ppf "$%d"
