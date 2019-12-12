open Util

type m = string list [@@deriving show]

type coordinate = (int * int) [@@deriving show]
type coordinate_list = coordinate list [@@deriving show]
type angle_and_point = (float * coordinate) [@@deriving show]
type angle_and_point_list = angle_and_point list [@@deriving show]
type angle_and_point_grouped_list =  angle_and_point_list list [@@deriving show]

let normalize coord =
  let (x, y) = coord in
  if x = 0 && y = 0 then (0, 0)
  else if x = 0 then (0, (y / abs y))
  else if y = 0 then ((x / abs x), 0)
  else let d = abs (gcd x y) in
    (x / d, y / d)

let coordinate_offset base other =
  let (x1, y1) = base in
  let (x2, y2) = other in
  (x2 - x1, y1 - y2)

let point_as_floats (x, y) = ((float_of_int x), (float_of_int y))
let pi = atan2 0. (-1.)
let angle point =
  let (up_x, up_y) = point_as_floats (0, (1)) in
  let (f_x, f_y) = point_as_floats point in
  let res = atan2 up_y up_x -. atan2 f_y f_x in
  if res < 0. then res +. 2. *. pi else res

let asteroids_visible_from coord maze =
  CCList.map (coordinate_offset coord) maze
  |> CCList.map normalize
  |> CCList.sort_uniq ~cmp: Stdlib.compare
  |> CCList.length
  |> (fun l -> l - 1)

let coordinates_of_maze maze = 
  CCList.foldi (fun coords y line ->
      CCList.foldi (fun coords x cell ->
          if cell = '#' then (x, y) :: coords else coords)
        coords (CCString.to_list line)) [] maze

let print_solution (count, (x, y)) =
  Printf.sprintf "%d asteroids visible from (%d, %d)" count x y
  |> print_endline

let find_station maze = 
  let coords = coordinates_of_maze maze in
  CCList.fold_left (fun (best, which) cur ->
      let result = asteroids_visible_from cur coords in
      if result > best then (result, cur) else (best, which)) (0, (0, 0)) coords
            
let solve maze =
  find_station maze
  |> print_solution

let solve_b maze which =
  let (_, station) = find_station maze in
  let (sx, sy) = station in
  let asteroids = coordinates_of_maze maze |> CCList.filter (fun (x, y) -> x != sx || y != sy) in
  let offsets = CCList.map (coordinate_offset station) asteroids in
  CCList.map (fun offset -> (angle offset, offset)) offsets
  |> CCList.group_by ~eq: (fun (a, _) (b, _) -> a = b)
    ~hash: (fun (a, _) -> (int_of_float (a *. 1e6)))
  |> CCList.map (CCList.sort Stdlib.compare)
  |> CCList.sort (fun a b -> Stdlib.compare (CCList.hd a) (CCList.hd b))
  |> interleave_lists
  |> (fun list -> CCList.nth list (which - 1))
  |> (fun (_, (x, y)) -> Printf.sprintf "Asteroid %d: (%d, %d)" which (sx + x) (sy - y))
  |> print_endline

let read_maze () =
  read_all read_line

let _ =
  let maze = read_maze () in
  time (fun () -> solve maze);
  time (fun () -> solve_b maze 200)
