let parse str = Scanf.sscanf str "%d,%d" Pair.make

let data =
  In_channel.with_open_text "adv09.txt" In_channel.input_lines
  |> List.map parse

let area (x1,y1) (x2,y2) =
  (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

let pair_areas points =
  let rec aux acc = function
    | [] -> acc
    | x::xs ->
       let add_area areas y = (area x y,x,y) :: areas in
       let acc' = List.fold_left add_area acc xs in
       aux acc' xs
  in
  aux [] points

let segments points =
  match points with
  | [] -> failwith "not enough points"
  | x::xs -> List.map2 Pair.make points (xs @ [x])

let intersects_inside ((px,py),(qx,qy)) ((ax,ay),(bx,by)) =
  (* vertical *)
  ax = bx &&
    min px qx < ax && ax < max px qx &&
      max ay by > min py qy && max py qy > min ay by ||
  (* horizontal *)
  ay = by &&
    min py qy < ay && ay < max py qy &&
      max ax bx > min px qx && max px qx > min ax bx

let is_red_green rect segs =
  List.for_all (Fun.negate (intersects_inside rect)) segs

let () =
  let greater (a,_,_) (b,_,_) = compare b a in
  let sorted_pairs = pair_areas data |> List.sort greater in
  let (p1,_,_) = List.hd sorted_pairs in
  let segs = segments data in
  let aux (_,p,q) = is_red_green (p,q) segs in
  let (p2,_,_) = List.find aux sorted_pairs in
  Printf.printf "%d\n%d\n" p1 p2
