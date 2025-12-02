open Containers

let data =
  let parse range = Scanf.sscanf range "%d-%d" (fun a b -> (a,b)) in
  In_channel.with_open_text "adv02.txt" In_channel.input_all
  |> String.trim |> String.split_on_char ',' |> List.map parse

let part1 n =
  let s = string_of_int n in
  let len = String.length s in
  len mod 2 = 0 &&
    let half = len / 2 in
    String.equal (String.sub s 0 half) (String.sub s half half)

let part2 n =
  let s = string_of_int n in
  let len = String.length s in
  let has_period p =
    len mod p = 0 &&
      let prefix = String.sub s 0 p in
      let check i = String.equal prefix (String.sub s (i * p) p) in
      List.range' 1 (len / p) |> List.for_all check
  in
  len > 1 && List.range 1 (len / 2) |> List.exists has_period

let fakes pred ranges =
  let sum_range acc (a,b) =
    Seq.(a -- b)
    |> Seq.filter pred
    |> Seq.fold_left (+) acc
  in
  List.fold_left sum_range 0 ranges

let () =
  Printf.printf "%d\n%d\n" (fakes part1 data) (fakes part2 data)
