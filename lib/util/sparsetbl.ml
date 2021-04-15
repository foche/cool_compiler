(* sparsetbl.ml *)

type t = {
    tbl : int array array;
    data : int array;
    log2 : int array;
    n : int;
  }

let fill_log2 log2 n =
  log2.(1) <- 0;
  for i = 2 to n - 1 do
    log2.(i) <- log2.(i / 2) + 1
  done

let argmin tbl data row1 col1 row2 col2 =
  let i = tbl.(row1).(col1) in
  let j = tbl.(row2).(col2) in
  if data.(i) < data.(j) then i else j

let preprocess_data tbl m n data =
  for col = 0 to n - 1 do
    tbl.(0).(col) <- col
  done;
  for row = 1 to m - 1 do
    for col = 0 to n - (1 lsl row) do
      let col2 = col + (1 lsl (row - 1)) in
      tbl.(row).(col) <-
        argmin
          tbl data
          (row - 1) col
          (row - 1) col2
    done
  done

let create data =
  let n = Array.length data in
  let log2 = Array.make (n + 1) 0 in
  fill_log2 log2 (n + 1);
  let m = log2.(n) + 1 in
  let tbl = Array.make_matrix m n (-10000) in
  preprocess_data tbl m n data;
  {
    tbl;
    data = Array.copy data;
    log2;
    n;
  }

let rec range_min tbl left right =
  match left <= right, left >= 0 && right < tbl.n with
  | false, _ -> range_min tbl right left
  | true, false ->
    Printf.sprintf
      "RMQ query is out of bounds: (%d, %d); element count: %d"
      left right
      tbl.n |>
    invalid_arg
  | true, true ->
    let row = tbl.log2.(right - left + 1) in
    let col2 = right - (1 lsl row) + 1 in
    let i = argmin tbl.tbl tbl.data row left row col2 in
    i, tbl.data.(i)
