(* sparsetbl.ml *)

open! StdLabels

type t = {
  tbl : Int.t Array.t Array.t;
  data : Int.t Array.t;
  log2 : Int.t Array.t;
}

let fill_log2 log2 n =
  for i = 2 to n - 1 do
    log2.(i) <- log2.(i / 2) + 1
  done

let argmin tbl ~data ~x1 ~y1 ~x2 ~y2 =
  let i = tbl.(x1).(y1) in
  let j = tbl.(x2).(y2) in
  if data.(i) < data.(j) then i else j

let fill_tbl tbl ~dimx ~dimy ~data =
  (* dimy = len data *)
  for y = 0 to dimy - 1 do
    tbl.(0).(y) <- y (* argmin data [y, y] is y *)
  done;
  for x = 1 to dimx - 1 do
    for y = 0 to dimy - (1 lsl x) do
      (* y2 = y + 2^(x - 1) *)
      let y2 = y + (1 lsl (x - 1)) in
      (*
        argmin data [y, y + 2^x] is
        argmin data (
          argmin data [y, y + 2^(x - 1)],
          argmin data [y + 2^(x - 1), y + 2^x])
       *)
      tbl.(x).(y) <- argmin tbl ~data ~x1:(x - 1) ~y1:y ~x2:(x - 1) ~y2
    done
  done

let create ~data =
  let dimy = Array.length data in
  let log2 = Array.make (dimy + 1) 0 in
  fill_log2 log2 (dimy + 1);
  let dimx = log2.(dimy) + 1 in
  let tbl = Array.make_matrix ~dimx ~dimy 0 in
  fill_tbl tbl ~dimx ~dimy ~data;
  { tbl; data = Array.copy data; log2 }

let rec range_min tbl ~left ~right =
  match (left <= right, left >= 0 && right < Array.length tbl.data) with
  | false, _ -> range_min tbl ~left:right ~right:left
  | true, false ->
      Printf.sprintf "RMQ query is out of bounds: (%d, %d); element count: %d"
        left right (Array.length tbl.data)
      |> invalid_arg
  | true, true ->
      let x = tbl.log2.(right - left + 1) in
      let y2 = right - (1 lsl x) + 1 in
      let i = argmin tbl.tbl ~data:tbl.data ~x1:x ~y1:left ~x2:x ~y2 in
      (i, tbl.data.(i))
