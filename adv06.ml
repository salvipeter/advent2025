let data =
  In_channel.with_open_text "adv06.txt" In_channel.input_lines

let parsed =
  List.map (fun line ->
      String.split_on_char ' ' line
      |> List.filter (Fun.negate String.is_empty)
    ) data

let compute_col tbl col =
  let open Fun in
  let n = List.length tbl in
  let values =
    List.take (n - 1) tbl
    |> List.map (flip List.nth col %> int_of_string)
  in
  let operation =
    match List.last 1 tbl |> List.hd |> flip List.nth col with
    | "+" -> ( + )
    | "*" -> ( * )
    |  _  -> failwith "invalid operation"
  in
  List.reduce_exn operation values

let parse_number chars =
  List.fold_left (fun acc c ->
      if Char.equal c ' ' then acc
      else acc * 10 + Char.to_int c - Char.to_int '0'
    ) 0 chars

let process_col tbl (result, nums) col =
  let cs = List.map (fun str -> str.[col]) tbl in
  if List.for_all (Char.equal ' ') cs then
    (result, nums)
  else
    let operation = List.hd (List.last 1 cs) in
    let n = parse_number (List.take (List.length cs - 1) cs) in
    match operation with
    | ' ' -> (result, n::nums)
    | '+' -> (result + List.fold_left ( + ) 0 (n::nums), [])
    | '*' -> (result + List.fold_left ( * ) 1 (n::nums), [])
    |  _  -> failwith "invalid operation"

let () =
  let p1 =
    let n = List.length (List.hd parsed) in
    let results = List.map (compute_col parsed) (List.range' 0 n) in
    List.fold_left (+) 0 results
  in
  let p2 =
    let n = String.length (List.hd data) in
    List.range_by ~step:(-1) (n - 1) 0
    |> List.fold_left (process_col data) (0, [])
    |> fst
  in
  Printf.printf "%d\n%d\n" p1 p2
