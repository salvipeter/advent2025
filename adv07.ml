let data =
  In_channel.with_open_text "adv07.txt" In_channel.input_lines

let count_beams rows part2 =
  let cache = Hashtbl.create (List.length rows) in
  let rec aux rows i j =
    match (Hashtbl.find_opt cache (i, j), rows) with
    | (Some result, _) -> if part2 then result else 0
    | (None, []) -> 0
    | (None, row::rest) when i >= 0 && i < String.length row ->
       let result =
         if Char.equal row.[i] '^' then
           1 + aux rows (i - 1) j + aux rows (i + 1) j
         else
           aux rest i (j + 1)
       in
       Hashtbl.add cache (i, j) result;
       result
    | (None, _) -> 0
  in
  let start = String.index (List.hd rows) 'S' in
  aux (List.tl rows) start 1

let () =
  Printf.printf "%d\n%d\n"
    (count_beams data false)
    (count_beams data true + 1)
