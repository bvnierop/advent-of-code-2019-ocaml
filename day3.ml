open Util

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
type wire = point list [@@deriving show]

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

module PointMap = CCMap.Make(struct type t = point let compare = Stdlib.compare end)

let wire_distances wire =
  CCList.foldi (fun acc index elem ->
      if PointMap.mem elem acc then
        acc
      else
        PointMap.add elem (index + 1) acc) PointMap.empty wire

let solve_b wire_a wire_b =
  let dist_a = wire_distances wire_a in
  let dist_b = wire_distances wire_b in
  let common_points = common_points wire_a wire_b in
  let min = list_min ~by: (fun pt ->
      PointMap.find pt dist_a + PointMap.find pt dist_b) common_points  in
  print_endline (Printf.sprintf "Closest intersection by wire distance is %s at %d"
                   (show_point min) (PointMap.find min dist_a + PointMap.find min dist_b))

let _ =
  let wire_a = wire_of_string (read_line ()) in
  let wire_b = wire_of_string (read_line ()) in
  time (fun () -> solve_a wire_a wire_b);
  time (fun () -> solve_b wire_a wire_b);
