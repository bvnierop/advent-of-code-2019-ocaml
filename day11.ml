open Util
open IntCodeComputer

module Color = struct
  type t = White | Black [@@deriving show]

  let of_int value =
    match value with
    | 0 -> Black
    | 1 -> White
    | _ -> failwith (Printf.sprintf "Invalid color code: %d" value)

  let to_int color =
    match color with
    | Black -> 0
    | White -> 1

  let to_string color =
    match color with
    | Black -> " "
    | White -> "#"
end

module Grid = struct
  type point = (int * int) [@@deriving show]
  type painted_list = (point * Color.t) list [@@deriving show]
  module PointMap = CCMap.Make(struct type t = point let compare = Stdlib.compare end)

  type t = {
    points: Color.t PointMap.t;
    min_x: int;
    min_y: int;
    max_x: int;
    max_y: int
  }

  let empty: t = {
    points = PointMap.empty;
    min_x = 0;
    min_y = 0;
    max_x = 0;
    max_y = 0
  }

  let paint color point grid =
    let (x, y) = point in
    {
      points = PointMap.add point color grid.points;
      min_x = min grid.min_x x;
      min_y = min grid.min_y y;
      max_x = max grid.min_x x;
      max_y = max grid.min_y y;
    }

  let painted grid =
    PointMap.fold (fun point color acc -> (point, color) :: acc) grid.points []

  let get_color point grid =
    PointMap.get_or ~default: Color.Black point grid.points

  let pp formatter grid = Format.pp_print_string formatter (show_painted_list (painted grid))

  let display grid =
    Gen.map (fun row ->
        Gen.map (fun col -> Color.to_string (get_color (col, row) grid)) (Gen.int_range grid.min_x (grid.max_x + 1))
        |> Gen.to_list
        |> CCString.concat "") (Gen.int_range grid.min_y (grid.max_y + 1))
    |> Gen.to_list
    |> CCString.concat "\n"

    
end

module Robot = struct
  type orientation = Up | Down | Left | Right [@@deriving show]
  type state = Paint | Move [@@deriving show]
  type t = {
    orientation: orientation;
    x: int;
    y: int;
    state: state;
    map: Grid.t
  } [@@deriving show]

  let initial = { orientation = Up; x = 0; y = 0; state = Paint; map = Grid.empty }

  let paint input robot =
    let color = Color.of_int input in
    { robot with map = Grid.paint color (robot.x, robot.y) robot.map }

  let turn_left robot =
    { robot with orientation =match robot.orientation with
          | Up -> Left
          | Left -> Down
          | Down -> Right
          | Right -> Up }

  let turn_right robot =
    { robot with orientation = match robot.orientation with
          | Up -> Right
          | Right -> Down
          | Down -> Left
          | Left -> Up }

  let turn input robot =
    match input with
    | 0 -> turn_left robot
    | 1 -> turn_right robot
    | _ -> failwith (Printf.sprintf "Invalid turn direction: %d" input)

  let move input robot =
    let turned = turn input robot in
    let (x, y) = match turned.orientation with
      | Up -> (robot.x, robot.y - 1)
      | Down -> (robot.x, robot.y + 1)
      | Left -> (robot.x - 1, robot.y)
      | Right -> (robot.x + 1, robot.y) in
    { turned with x = x; y = y }

  let with_input input robot =
    match robot.state with
    | Paint -> { (paint input robot) with state = Move }
    | Move -> { (move input robot) with state = Paint }

  let current_color robot = Grid.get_color (robot.x, robot.y) robot.map
end

let simulate starting_color memory =
  let output_handler = (fun output robot program ->
      match robot with
      | None -> (robot, program)
      | Some r ->
        let acted = Robot.with_input output r in
        match acted.state with
        | Paint ->
          let gen = acted |> Robot.current_color |> Color.to_int |> Gen.singleton in
          (Some acted, program_add_input gen program)
        | Move -> (Some acted, program)) in
  let program = program_with_output output_handler (program_make ~input: (Gen.singleton (Color.to_int starting_color)) memory)
      ~initial: (Some Robot.initial) in
  let executed = program_execute ~verbose: false ~print_outputs: false program in
  match executed.state with
   | None -> Grid.empty
   | Some robot -> robot.map

let solve memory =
  simulate Color.Black memory
  |> Grid.painted
  |> CCList.length
  |> string_of_int
  |> print_endline

let draw memory =
  simulate Color.White memory
  |> Grid.display
  |> print_endline

let _ =
  let memory = read_memory () in
  time (fun () -> solve memory);
  time (fun () -> draw memory)
