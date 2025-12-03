let data =
  let digit c = Char.code c - Char.code '0' in
  In_channel.with_open_text "adv03.txt" In_channel.input_lines
  |> List.map CCFun.(String.to_list %> List.map digit)

let activation batteries bank =
  let rec aux acc xs k =
    if k < 0 then
      acc
    else
      let m = List.reduce_exn max (List.rev xs |> List.drop k) in
      let xs' = List.drop_while ((<>) m) xs |> List.tl in
      aux (acc * 10 + m) xs' (k - 1)
  in
  aux 0 bank (batteries - 1)

let joltage batteries banks =
  List.map (activation batteries) banks |> List.fold_left (+) 0

let () =
  Printf.printf "%d\n%d\n" (joltage 2 data) (joltage 12 data)
