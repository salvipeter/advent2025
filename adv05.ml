let parse xs =
  let ranges, ids = List.take_drop_while (String.(<>) "") xs in
  let parse_range range = Scanf.sscanf range "%d-%d" (fun a b -> (a, b)) in
  (List.map parse_range ranges, List.map int_of_string (List.tl ids))

let data =
  In_channel.with_open_text "adv05.txt" In_channel.input_lines
  |> List.map String.trim |> parse

let fresh ranges ids =
  let inrange x (a, b) = a <= x && x <= b in
  List.count (fun id -> List.exists (inrange id) ranges) ids

let merge_ranges (xa, xb) (ya, yb) =
  if xb + 1 < ya || yb + 1 < xa then
    None
  else
    Some (min xa ya, max xb yb)

let range_length (a, b) = b - a + 1

let rec combined_length = function
  | [] -> 0
  | (x::xs) ->
     (* Try to merge x with all remaining ranges *)
     let aux (x', acc) y =
       match merge_ranges x' y with
       | None     -> (x', y::acc)
       | Some x'' -> (x'', acc)
     in
     let x', xs' = List.fold_left aux (x, []) xs in
     (* If there was no change, continue, otherwise try again *)
     if Equal.poly x x' then
       range_length x + combined_length xs'
     else
       combined_length (x'::xs')

let () =
  let ranges, ids = data in
  Printf.printf "%d\n%d\n" (fresh ranges ids) (combined_length ranges)
