let read_line_opt () =
  try Some (read_line ())
  with End_of_file -> None

let read_all reader =
  let rec read acc =
    try let value = reader () in read (value :: acc)
    with _ -> List.rev acc in
  read []

let read_all_opt reader_opt =
  let rec read acc =
    match reader_opt () with
    | Some value -> read (value :: acc)
    | None -> List.rev acc in
  read []

let read_next_int () =
  Scanf.scanf "%d" (fun i -> i)

let read_next_int_opt () =
  try Some(read_next_int ())
  with _ -> None

let read_all_lines () =
  read_all read_line

let read_char () =
  Scanf.scanf "%c" (fun c -> c)

let read_char_opt () =
  try Some(read_char ())
  with _ -> None

let identity x = x

let list_min ~(by: 'a -> 'b) list =
  let rec find_min current_min current_min_value remaining =
    match remaining with
    | [] -> current_min
    | x::xs -> let current_value = by x in
      if current_value < current_min_value then
        find_min x current_value xs
      else
        find_min current_min current_min_value xs
  in
  let first_item = CCList.hd list in
  find_min first_item (by first_item) list

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Printf.printf "Execution time: %f seconds\n"
                (Unix.gettimeofday () -. t);
  res
          

(* TODO: Extract above to code reuse *)

type instruction =
  | Up of int
  | Down of int
  | Left of int
  | Right of int
[@@deriving show]

let instruction_of_string str =
  Scanf.sscanf str "%c%d" (fun dir dist ->
      match dir with
      | 'U' -> Up dist
      | 'D' -> Down dist
      | 'L' -> Left dist
      | 'R' -> Right dist
      | _ -> failwith (Printf.sprintf "Input error: %c" dir))

type point = (int * int) [@@deriving show]
type wire_part = (point * point) [@@deriving show]
type wire = point list [@@deriving show]
type wire2 = wire_part list [@@deriving show]
type line_equation = (int * int * int) [@@deriving show]

let line_equation_of_points ((x1, y1), (x2, y2)) =
  let a = y2 - y1 in
  let b = x1 - x2 in
  let c = a * x1 + b * y1 in
  (a, b, c)

let determinant l1 l2 =
  let (a1, b1, _) = l1 in
  let (a2, b2, _) = l2 in
  a1 * b2 - a2 * b1

let intersection_opt wire_part_a wire_part_b =
  let l1 = line_equation_of_points wire_part_a in
  let l2 = line_equation_of_points wire_part_b in
  let det = determinant l1 l2 in
  if det = 0 then None
  else let (a1, b1, c1) = l1 in
    let (a2, b2, c2) = l2 in
    let x = (b2 * c1 - b1 * c2) / det in
    let y = (a1 * c2 - a2 * c1) / det in
    Some (x, y)

let manhattan_distance (x1, y1) (x2, y2) =
  abs (x1 - x2) + abs (y1 - y2)

let wire_of_instructions instructions =
  let make_x_range x y_start y_end =
    CCList.range y_start y_end
    |> CCList.map (fun y -> (x, y)) in
  let make_y_range y x_start x_end =
    CCList.range x_start x_end
    |> CCList.map (fun x -> (x, y)) in
  let rec iter remaining cur_x cur_y accumulator =
    match remaining with
    | [] -> List.flatten (List.rev accumulator)
    | x::xs -> match x with
      | Up dist -> iter xs cur_x (cur_y + dist)
                     (make_x_range cur_x (cur_y + 1) (cur_y + dist) :: accumulator)
      | Down dist -> iter xs cur_x (cur_y - dist)
                     (make_x_range cur_x (cur_y - 1) (cur_y - dist) :: accumulator)
      | Left dist -> iter xs (cur_x - dist) cur_y
                     (make_y_range cur_y (cur_x - 1) (cur_x - dist) :: accumulator)
      | Right dist -> iter xs (cur_x + dist) cur_y
                     (make_y_range cur_y (cur_x + 1) (cur_x + dist) :: accumulator) in
  iter instructions 0 0 []


let wire_of_string str =
  CCString.split_on_char ',' str
  |> CCList.map instruction_of_string
  |> wire_of_instructions

let common_points wire_a wire_b =
  let uniq_a = wire_a |> CCList.sort_uniq ~cmp: Stdlib.compare in
  let uniq_b = wire_b |> CCList.sort_uniq ~cmp: Stdlib.compare in
  uniq_a @ uniq_b
  |> CCList.group_by
  |> CCList.filter (fun l -> CCList.length l > 1)
  |> CCList.map CCList.hd

let solve_a wire_a wire_b =
  let common_points = common_points wire_a wire_b in
  let min = list_min ~by: (fun c -> manhattan_distance c (0, 0)) common_points in
  print_endline (Printf.sprintf "Closest intersection is %s, at %d" (show_point min) (manhattan_distance min (0, 0)))

let _ =
  let wire_a = wire_of_string (read_line ()) in
  let wire_b = wire_of_string (read_line ()) in
  solve_a wire_a wire_b
