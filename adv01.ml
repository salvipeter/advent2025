type combination = Left of int | Right of int

let parse str =
  let n = int_of_string (String.drop 1 str) in
  match str.[0] with
  | 'L' -> Left n
  | 'R' -> Right n
  |  _  -> failwith "invalid input"

let data =
  In_channel.with_open_text "adv01.txt" In_channel.input_lines
  |> List.map parse

let zeros part2 dial combs =
  let f (dial, score) turn =
    let clicks, next =
      match turn with
      | Left  n ->
         let turns = (if dial = 0 then n else 100 - dial + n) / 100 in
         (turns, Int.rem (dial - n) 100)
      | Right n ->
         let turns = (dial + n) / 100 in
         (turns, (dial + n) mod 100)
    in
    let delta = if part2 then clicks else if next = 0 then 1 else 0 in
    (next, score + delta)
  in
  List.fold_left f (dial, 0) combs |> snd

let solve part2 = zeros part2 50 data

let () =
  Printf.printf "%d\n%d\n" (solve false) (solve true)
