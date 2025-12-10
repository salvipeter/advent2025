let parse_lights =
  Parse.(U.list ~sep:"" (char '#' <|> char '.' >|= (Char.equal '#')))
let parse_buttons =
  Parse.(many (skip_space *> U.list ~start:"(" ~stop:")" ~sep:"," U.int))
let parse_joltages =
  Parse.(U.list ~start:"{" ~stop:"}" ~sep:"," U.int)
let parse =
  let open Parse in
  map3 (fun l b j -> (l,b,j))
    (parse_lights <* skip_space)
    (parse_buttons <* skip_space)
    parse_joltages

let data =
  In_channel.with_open_text "adv10.txt" In_channel.input_lines
  |> List.map (Parse.parse_string_exn parse)

let rec subsets = function
  |  []   -> [[]]
  | x::xs ->
     let rest = subsets xs in
     rest @ List.map (fun ys -> x::ys) rest

let good_subset goal xs =
  List.for_all (fun i ->
      let count = List.count (fun x -> List.mem i x) xs in
      Bool.equal (List.nth goal i) (count mod 2 = 1)
    ) (List.range' 0 (List.length goal))

let compare_lengths x y = compare (List.length x) (List.length y)

let minimal_pushes_lights buttons goal =
  let sets = subsets buttons |> List.sort compare_lengths in
  List.find (good_subset goal) sets |> List.length

let prune_buttons goal buttons =
  List.filter (List.for_all (fun i -> List.nth goal i > 0)) buttons

let fixed_first goal buttons =
  let rec aux i acc = function
    | [] -> failwith "internal error"
    | x::xs -> if List.mem i x then
                 x :: List.rev acc @ xs
               else
                 aux i (x::acc) xs
  in
  match List.find_opt (fun i ->
            List.nth goal i > 0 && List.count (List.mem i) buttons = 1
          ) (List.range' 0 (List.length goal))
  with
  | None   -> buttons
  | Some i -> aux i [] buttons

let subtract xs is n =
  let rec aux i = function
    | [] -> []
    | x::xs -> let x' = if List.mem i is then (x - n) else x in
               x' :: aux (i + 1) xs
  in
  aux 0 xs

let minimal_pushes_joltage buttons goal =
  let rec aux n least buttons goal =
    if Option.map_or ~default:false ((>) n) least then
      None
    else if List.for_all ((=) 0) goal then
      Some n
    else match buttons with
         | [] -> None
         | b::rest ->
            let limit = List.map (List.nth goal) b |> List.reduce_exn min in
            let only_here = List.filter (fun i ->
                                not (List.exists (List.mem i) rest)
                              ) b
            in
            let fixed = List.map (List.nth goal) only_here in
            if List.length fixed > 1 && List.exists ((<>) (List.hd fixed)) (List.tl fixed)
               || not (List.is_empty fixed) && List.hd fixed > limit
               || List.exists (fun i ->
                      List.nth goal i > 0 && not (List.exists (List.mem i) buttons)
                    ) (List.range' 0 (List.length goal))
            then
              None
            else
              let start = if List.is_empty fixed then 0 else List.hd fixed in
              let stop = if List.is_empty fixed then limit else List.hd fixed in
              let rest' = if stop = limit then
                            prune_buttons goal rest |> fixed_first goal
                          else
                            fixed_first goal rest
              in
              let push best i =
                Option.(aux (n + i) best rest' (subtract goal b i) <+> best)
              in
              List.fold_left push least (List.range_by ~step:(-1) stop start)
  in
  let buttons' = List.sort compare_lengths buttons |> List.rev in
  aux 0 None buttons' goal

let () =
  let p1 = List.map (fun (blinkenlights,buttons,_) ->
               minimal_pushes_lights buttons blinkenlights
             ) data
           |> List.fold_left (+) 0
  in
  let p2 = List.map (fun (_,buttons,joltages) ->
               minimal_pushes_joltage buttons joltages
               |> Option.get_exn_or "no good configuration"
               (* |> (fun x -> Printf.printf "%d\n%!" x; x) *)
             ) data
           |> List.fold_left (+) 0
  in
  Printf.printf "%d\n%d\n" p1 p2
