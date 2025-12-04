(* Creates a nested array of characters *)
let make_map xs =
  let n = List.length xs in
  let m = String.length (List.hd xs) in
  Array.init_matrix n m (fun i j -> (List.nth xs i).[j])

let data =
  In_channel.with_open_text "adv04.txt" In_channel.input_lines
  |> List.map String.trim |> make_map

let pos tbl i j =
  Option.(Array.get_safe tbl i >>= Fun.flip Array.get_safe j)

(* Number of adjacent non-period characters *)
let adjacent tbl i j =
  let aux acc (k,l) =
    match pos tbl (i + k) (j + l) with
    | None     -> acc
    | Some '.' -> acc
    | Some  _  -> acc + 1
  in
  List.fold_left aux 0 [(-1,-1);(0,-1);(1,-1);(-1,0);(1,0);(-1,1);(0,1);(1,1)]

(* Replaces each '#' with a period *)
let clear tbl =
  let aux c = if Char.equal c '#' then '.' else c in
  Array.iter (Array.map_inplace aux) tbl

(* Marks removable rolls with '#' and returns their count *)
let count_and_mark tbl =
  let count = ref 0 in
  let aux i j c =
    if not (Char.equal c '.') && adjacent tbl i j < 4 then (
      incr count;
      tbl.(i).(j) <- '#'
    )
  in
  Array.iteri (fun i row -> Array.iteri (aux i) row) tbl;
  !count

let rec moveable tbl once =
  let count = count_and_mark tbl in
  if once || count = 0 then
    count
  else (
    clear tbl;
    count + moveable tbl once
  )

let () =
  let p1 = moveable data true in
  let p2 = moveable data false in
  Printf.printf "%d\n%d\n" p1 p2
