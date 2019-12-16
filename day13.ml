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

  type t = Tile.t PointMap.t

  let empty = PointMap.empty

  let points grid = PointMap.to_list grid

  let get _point _grid = Tile.Empty
  let set point tile grid = PointMap.add point tile grid
  let to_string _grid = ""
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
  } 

  let empty = { input_state = Empty; grid = Grid.empty }

  let next_input_state input arcade = 
    match arcade.input_state with
    | Empty -> X input
    | X x -> Y (x, input)
    | Y (x, y) -> Tile (x, y, input)
    | Tile (_, _, _) -> failwith "Unexpected process input state."

  let update_grid x y tile arcade =
    { arcade with grid = Grid.set (x, y) (Grid.Tile.of_int tile) arcade.grid }

  let process_input input arcade =
    let input_state = next_input_state input arcade in
    match input_state with
    | Empty | X _ | Y _ -> { arcade with input_state = input_state }
    | Tile (x, y, tile) -> { (update_grid x y tile arcade) with input_state = Empty }
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
  | Some arcade -> Grid.points arcade.grid |> CCList.filter (fun (_, tile) -> match tile with | Grid.Tile.Block -> true | _ -> false) |> CCList.length

let solve memory =
  simulate memory
  |> string_of_int
  |> print_endline


let _ =
  let memory = read_memory () in
  time (fun () -> solve memory)
