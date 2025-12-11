let parse =
  let open Parse in
  map2 Pair.make (U.word <* char ':') (many (char ' ' *> U.word))

let data =
  In_channel.with_open_text "adv11.txt" In_channel.input_lines
  |> List.map (Parse.parse_string_exn parse)

let count_paths connections start stop visits =
  let cache = Hashtbl.create (List.length connections) in
  let rec aux needed node =
    let key = (node, needed) in
    match Hashtbl.find_opt cache key with
    | Some result -> result
    | None ->
       if String.(=) node stop then
         if List.is_empty needed then 1 else 0
       else
         match List.assoc_opt ~eq:String.(=) node connections with
         | None -> 0
         | Some next ->
            let needed' = List.filter (String.(<>) node) needed in
            let result = List.map (aux needed') next
                         |> List.fold_left (+) 0
            in
            Hashtbl.add cache key result;
            result
  in
  aux visits start

let () =
  let p1 = count_paths data "you" "out" [] in
  let p2 = count_paths data "svr" "out" ["dac";"fft"] in
  Printf.printf "%d\n%d\n" p1 p2
