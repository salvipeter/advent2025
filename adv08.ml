let parse str = Scanf.sscanf str "%d,%d,%d" (fun x y z -> (x,y,z))

let data =
  In_channel.with_open_text "adv08.txt" In_channel.input_lines
  |> List.map parse

let squared_distance (x1,y1,z1) (x2,y2,z2) =
  let sq n = n * n in
  sq (x1 - x2) + sq (y1 - y2) + sq (z1 - z2)

let pair_dists points =
  let rec outer acc i = function
    | [] -> acc
    | x::xs ->
       let rec inner acc j = function
         | [] -> acc
         | y::ys ->
            let d = squared_distance x y in
            inner ((d,i,j) :: acc) (j + 1) ys
       in
       outer (inner acc (i + 1) xs) (i + 1) xs
  in
  outer [] 0 points

let update_groups groups i j =
  let eq = Equal.physical in
  let find_group x = List.find_opt (List.mem x) groups in
  let remove_group = List.remove_one ~eq:eq in
  match find_group i, find_group j with
  | None,   None   ->  [i; j]  :: groups
  | Some a, None   -> (j :: a) :: remove_group a groups
  | None,   Some b -> (i :: b) :: remove_group b groups
  | Some a, Some b ->
     if eq a b then
       groups
     else
       (a @ b) :: remove_group a groups |> remove_group b

let connect_pairs xs =
  List.fold_left (fun acc (_,i,j) -> update_groups acc i j) [] xs

let last_pair n xs =
  let rec aux groups = function
    | [] -> failwith "internal error"
    | (_,i,j) :: rest ->
       match update_groups groups i j with
       | [xs] when List.length xs = n -> (i,j)
       | groups' -> aux groups' rest
  in
  aux [] xs

let () =
  let less (a,_,_) (b,_,_) = compare a b in
  let sorted_pairs = pair_dists data |> List.sort less in
  let p1 = List.take 1000 sorted_pairs |> connect_pairs
           |> List.map List.length |> List.sort (Fun.flip compare)
           |> List.take 3 |> List.fold_left ( * ) 1
  in
  let i, j = last_pair (List.length data) sorted_pairs in
  let (x1,_,_), (x2,_,_) = List.nth data i, List.nth data j in
  let p2 = x1 * x2 in
  Printf.printf "%d\n%d\n" p1 p2
