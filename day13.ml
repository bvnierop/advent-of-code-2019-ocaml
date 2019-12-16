open Util
open IntCodeComputer

module Grid = struct
  module Tile = struct
    type t =
      | Empty
      | Wall
      | Block
      | Paddle
      | Ball

    let of_int i =
      match i with
      | 0 -> Empty
      | 1 -> Wall
      | 2 -> Block
      | 3 -> Paddle
      | 4 -> Ball
      | _ -> failwith (Printf.sprintf "Invalid tile: %d" i)

    let to_string tile =
      match tile with
      | Empty -> " "
      | Wall -> "#"
      | Block -> "@"
      | Paddle -> "_"
      | Ball -> "o"
  end
  module Point = struct type t = (int * int) let compare = Stdlib.compare end
  module PointMap = CCMap.Make(Point)

  type t = {
    map: Tile.t PointMap.t;
    paddle: Point.t option;
    ball: Point.t option;
  }

  let empty = { map = PointMap.empty; ball = None; paddle = None }

  let points grid = PointMap.to_list grid.map

  let get point grid = PointMap.get_or ~default: Tile.Empty point grid.map

  let store_positions point tile grid =
    match tile with
    | Tile.Ball -> { grid with ball = Some point }
    | Tile.Paddle -> { grid with paddle = Some point }
    | _ -> grid

  let set point tile grid =
    store_positions point tile
      { grid with map = PointMap.add point tile grid.map }

  let to_string grid =
    let (min_x, min_y), _ = PointMap.min_binding grid.map in
    let (max_x, max_y), _ = PointMap.max_binding grid.map in
    Gen.map (fun row ->
        Gen.map (fun col -> Tile.to_string (get (col, row) grid)) (Gen.int_range min_x (max_x + 1))
        |> Gen.to_list
        |> CCString.concat "") (Gen.int_range min_y (max_y + 1))
    |> Gen.to_list
    |> CCString.concat "\n"

end

module Arcade = struct
  type input_state =
    | Empty
    | X of int
    | Y of (int * int)
    | Tile of (int * int * int) [@@deriving show]

  type t = {
    input_state: input_state;
    grid: Grid.t;
    score: int;
  } 

  let empty = { input_state = Empty; grid = Grid.empty; score = 0 }

  let next_input_state input arcade = 
    match arcade.input_state with
    | Empty -> X input
    | X x -> Y (x, input)
    | Y (x, y) -> Tile (x, y, input)
    | Tile (_, _, _) -> failwith "Unexpected process input state."

  let update_grid x y tile arcade =
    match (x, y) with
    | -1, 0  -> { arcade with score = tile }
    | _, _ -> { arcade with grid = Grid.set (x, y) (Grid.Tile.of_int tile) arcade.grid }

  let process_input input arcade =
    let input_state = next_input_state input arcade in
    match input_state with
    | Empty | X _ | Y _ -> { arcade with input_state = input_state }
    | Tile (x, y, tile) -> { (update_grid x y tile arcade) with input_state = Empty }

  let joystick_position arcade =
    match arcade.grid.ball, arcade.grid.paddle with
    | (Some (bx, _), Some (px, _)) -> Stdlib.compare bx px
      | _ -> failwith "Failed to keep track of ball and paddle"

  let to_string arcade =
    Printf.sprintf "%s\n\nScore: %d" (Grid.to_string arcade.grid) (arcade.score)
end

let simulate memory =
  let arcade_handler = (fun output (arcade_opt: Arcade.t option) program ->
      match arcade_opt with
      | None -> failwith "No state given"
      | Some arcade -> ((Some (Arcade.process_input output arcade)), program)) in
  let program = program_with_output arcade_handler (program_make ~input: (Gen.empty) memory)
      ~initial: (Some Arcade.empty) in
  let executed = program_execute ~verbose: false ~print_outputs: false program in
  match executed.state with
  | None -> 0
  | Some arcade ->
    Grid.points arcade.grid |> CCList.filter (fun (_, tile) -> match tile with | Grid.Tile.Block -> true | _ -> false) |> CCList.length

let solve memory =
  simulate memory
  |> string_of_int
  |> print_endline

let play memory =
  let paid_memory = memory_update 0 2 memory in
  let arcade_handler = (fun output (arcade_opt: Arcade.t option) program ->
      match arcade_opt with
      | None -> failwith "No state given"
      | Some arcade -> ((Some (Arcade.process_input output arcade)), program)) in
  let program = program_with_output arcade_handler (program_make ~input: (Gen.empty) paid_memory)
      ~initial: (Some Arcade.empty) in
  let rec run prog =
    let next_state = program_execute prog in
    match next_state.run_state with
    | WaitingForInput -> (match next_state.state with
        | None -> failwith "No state given"
        | Some arcade ->
          (* print_endline (Arcade.to_string arcade); *)
          run (program_with_input_gen
                 (Gen.singleton (Arcade.joystick_position arcade)) next_state))
      | Terminated -> (match next_state.state with
        | None -> failwith "No state given"
        | Some arcade ->
          print_endline (Arcade.to_string arcade);
          arcade.score)
      | _ -> failwith "Unexpected run state" in
  print_endline (string_of_int (run program))

let _ =
  let memory = read_memory () in
  time (fun () -> solve memory);
  time (fun () -> play memory)
