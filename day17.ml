open Util
open IntCodeComputer

module Tile = struct
  type t =
    | Scaffold
    | Space

  let of_int i =
    match char_of_int i with
    | '#' -> Scaffold
    | '.' -> Space
    | '^' 
    | 'v' 
    | '<' 
    | '>' -> Scaffold
    | c -> failwith (Printf.sprintf "Invalid tile: %c" c)

  let show tile =
    match tile with
    | Scaffold -> "#"
    | Space -> "."
end

module Grid = struct
  type row = Tile.t CCPersistentArray.t 

  type t = row CCPersistentArray.t

  let empty: t = CCPersistentArray.make 1 (CCPersistentArray.make 0 Tile.Space)

  let get_row row_idx grid = CCPersistentArray.get grid row_idx

  let row_count grid = CCPersistentArray.length grid
  let col_count grid = CCPersistentArray.length (get_row 0 grid)

  let in_range row col grid =
    row >= 0 && row < row_count grid && col >= 0 && col < col_count grid

  let get_tile row_idx column_idx grid =
    if in_range row_idx column_idx grid then
      CCPersistentArray.get (get_row row_idx grid) column_idx
    else
      Tile.Space

  let last_row_index grid = (CCPersistentArray.length grid - 1)
  let set_row row_idx row grid : t = CCPersistentArray.set grid row_idx row


  let add_to_row row_idx tile grid : t =
    let row = get_row row_idx grid in
    let updated_row = CCPersistentArray.append row (CCPersistentArray.make 1 tile) in
    set_row row_idx updated_row grid

  let add_to_last_row tile grid : t =
    add_to_row (last_row_index grid) tile grid

  let trim grid =
    CCPersistentArray.to_list grid
    |> CCList.filter (fun row -> CCPersistentArray.length row > 0)
    |> CCPersistentArray.of_list

  let empty_row: t = (CCPersistentArray.make 1 (CCPersistentArray.make 0 Tile.Space))

  let add_input i (grid: t) : t =
    match i with
    | 10 -> CCPersistentArray.append grid empty_row
    | i -> add_to_last_row (Tile.of_int i) grid

  let fold fn initial grid =
    let rows = CCPersistentArray.length grid in
    let cols = CCPersistentArray.length (get_row 0 grid) in
    Gen.fold (fun acc row ->
        Gen.fold (fun acc col -> fn acc row col (get_tile row col grid)) acc
          (Gen.int_range 0 (cols - 1))) initial
      (Gen.int_range 0 (rows - 1))

  let row_to_string (row: row) : string = CCPersistentArray.fold_left (fun str tile -> str ^ (Tile.show tile)) "> " row
  let to_string (grid: t) : string = CCPersistentArray.fold_left (fun str row -> str ^ "\n" ^ (row_to_string row)) "" grid
end

module Robot = struct
  type orientation = North | East | South | West
  type t = {
    row: int;
    column: int;
    orientation: orientation;
  }
  let empty = { row = 0; column = 0; orientation = North }

  let step grid robot =
    let new_robot = match robot.orientation with
      | North -> { robot with row = robot.row - 1 }
      | East -> { robot with column = robot.column + 1 }
      | South -> { robot with row = robot.row + 1 }
      | West -> { robot with column = robot.column - 1 } in
    match Grid.get_tile new_robot.row new_robot.column grid with
    | Tile.Scaffold -> Some new_robot
    | _ -> None
      

  let turn direction robot =
    match direction, robot.orientation with
    | `Right, North -> { robot with orientation = East }
    | `Right, East -> { robot with orientation = South }
    | `Right, South -> { robot with orientation = West }
    | `Right, West -> { robot with orientation = North }
    | `Left, North -> { robot with orientation = West }
    | `Left, West -> { robot with orientation = South }
    | `Left, South -> { robot with orientation = East }
    | `Left, East -> { robot with orientation = North }

  let move direction grid robot =
    match direction with
    | `Straight -> step grid robot
    | `Right -> step grid (turn `Right robot)
    | `Left -> step grid (turn `Left robot)

  let show robot =
    match robot.orientation with
    | North -> "^"
    | East -> ">"
    | South -> "v"
    | West -> "<"

  let make row column orientation =
    { row = row;
      column = column;
      orientation = match orientation with
        | '^' -> North
        | '>' -> East
        | 'v' -> South
        | '<' -> West
        | _ -> failwith "Invalid orientation" }

end

module Instruction = struct
  type t = | Straight of int | Left | Right [@@deriving show]

  let show instruction =
    match instruction with
    | Left -> "L"
    | Right -> "R"
    | Straight n -> string_of_int n

  let show_list instructions =
    CCList.map show instructions
    |> CCString.concat ","
end

module Scaffolding = struct
  type t = {
    grid: Grid.t;
    robot: Robot.t;
  }

  let empty = { grid = Grid.empty; robot = Robot.empty }

  let add_input i scaffolding =
    let with_robot = match char_of_int i with
      | '^' | 'v' | '<' | '>' -> let row = Grid.last_row_index scaffolding.grid in
        let column = CCPersistentArray.length (Grid.get_row row scaffolding.grid) in
        { scaffolding with robot = Robot.make row column (char_of_int i) }
      | _ -> scaffolding in
   { with_robot with grid = Grid.add_input i scaffolding.grid }

  let cleanup scaffolding = { scaffolding with grid = Grid.trim scaffolding.grid }

  let step_robot_opt scaffolding =
    CCList.fold_while (fun _ dir ->
        match Robot.move dir scaffolding.grid scaffolding.robot with
        | Some new_robot -> ((Some new_robot, Some dir), `Stop)
        | None -> ((None, None), `Continue)) (None, None) [`Straight; `Left; `Right]
    |> (fun robot_opt ->
        match robot_opt with
        | (None, None) -> None
        | (None, Some _) | (Some _, None) -> failwith "IMPOSSIBLE STEP RESULT"
        | (Some robot, Some direction) -> Some ({ scaffolding with robot = robot }, direction))

  let show scaffolding =
    Grid.fold (fun acc row col tile ->
        let next = if (row, col) = (scaffolding.robot.row, scaffolding.robot.column) then Robot.show scaffolding.robot
          else Tile.show tile in
        let sep = if col = Grid.col_count scaffolding.grid - 1 then "\n" else "" in
        acc ^ next ^ sep) "" scaffolding.grid

  let find_path scaffolding =
    let rec find path forward_count current_state =
      (* print_endline (show current_state); *)
      match step_robot_opt current_state with
      | None -> CCList.tl (CCList.rev ((Instruction.Straight forward_count) :: path))
      | Some (next_state, last_move) ->
        match last_move with
        | `Left -> find (Instruction.Left :: (Straight forward_count :: path)) 1 next_state
        | `Right -> find (Right :: (Straight forward_count :: path)) 1 next_state
        | `Straight -> find path (forward_count + 1) next_state
    in
    find [] 0 scaffolding
end

let test_grid = [
  46;46;35;46;46;46;46;46;46;46;46;46;46;10;
  46;46;35;46;46;46;46;46;46;46;46;46;46;10;
  35;35;35;35;35;35;35;46;46;46;35;35;35;10;
  35;46;35;46;46;46;35;46;46;46;35;46;35;10;
  35;35;35;35;35;35;35;35;35;35;35;35;35;10;
  46;46;35;46;46;46;35;46;46;46;35;46;46;10;
  46;46;35;35;35;35;35;46;46;46;35;46;46;10;
]

let test_grid_2 = [
  35;35;35;35;35;35;35;46;46;46;35;35;35;35;35;10;
  35;46;46;46;46;46;35;46;46;46;35;46;46;46;35;10;
  35;46;46;46;46;46;35;46;46;46;35;46;46;46;35;10;
  46;46;46;46;46;46;35;46;46;46;35;46;46;46;35;10;
  46;46;46;46;46;46;35;46;46;46;35;35;35;46;35;10;
  46;46;46;46;46;46;35;46;46;46;46;46;35;46;35;10;
  94;35;35;35;35;35;35;35;35;46;46;46;35;46;35;10;
  46;46;46;46;46;46;35;46;35;46;46;46;35;46;35;10;
  46;46;46;46;46;46;35;35;35;35;35;35;35;35;35;10;
  46;46;46;46;46;46;46;46;35;46;46;46;35;46;46;10;
  46;46;46;46;35;35;35;35;35;35;35;35;35;46;46;10;
  46;46;46;46;35;46;46;46;35;46;46;46;46;46;46;10;
  46;46;46;46;35;46;46;46;35;46;46;46;46;46;46;10;
  46;46;46;46;35;46;46;46;35;46;46;46;46;46;46;10;
  46;46;46;46;35;35;35;35;35;46;46;46;46;46;46;10;
]

let test_maze grid =
  CCList.fold_left (fun scaffolding tile -> Scaffolding.add_input tile scaffolding) Scaffolding.empty grid |> Scaffolding.cleanup

let program_return x program = (Some x, program)

let generate_maze program =
  let output_handler = (fun output scaffolding_opt program ->
      match scaffolding_opt with
      | None -> failwith "Set a state"
      | Some scaffolding -> program_return (Scaffolding.add_input output scaffolding) program) in
  let prog = program_with_output ~initial: (Some Scaffolding.empty) output_handler program in
  let executed = program_execute prog in
  CCOpt.get_exn executed.state |> Scaffolding.cleanup


let solve program =
  let maze = generate_maze program in
  print_endline (Scaffolding.show maze);
  Grid.fold (fun acc row col tile ->
      if row = 0 || col = 0 || row = Grid.row_count maze.grid - 1 || col = Grid.col_count maze.grid - 1 then acc
      else let north = Grid.get_tile (row - 1) col maze.grid in
        let south = Grid.get_tile (row + 1) col maze.grid in
        let east = Grid.get_tile row (col + 1) maze.grid in
        let west = Grid.get_tile row (col - 1) maze.grid in
        let all_the_same = CCList.uniq ~eq: Stdlib.(=) [tile; north; south; east; west;] |> CCList.length |> (fun l -> l = 1) in
        if tile = Tile.Scaffold && all_the_same then acc + (row * col)
        else acc) 0 maze.grid
  |> string_of_int |> print_endline

module LRS = struct
  module PairMap = CCMap.Make(struct type t = (int * int) let compare = Stdlib.compare end)

  let get i j map = PairMap.get_or ~default: 0 (i, j) map
  let set i j value map = PairMap.add (i, j) value map

  let lrs ?(eq = Stdlib.(=)) list =
    let lengths = CCList.foldi (fun lcs i elt_i ->
        CCList.foldi (fun lcs j0 elt_j ->
            let j = j0 + i in
            if eq elt_i elt_j && get i j lcs < (j - i) then
              set (i + 1) (j + 1) ((get i j lcs) + 1) lcs
            else
              set (i + 1) (j + 1) 0 lcs
          ) lcs (CCList.drop (i + 1) list)
      ) PairMap.empty list in
    let best_length, best_i = PairMap.fold (fun (i, _) length (best_length, best_i) ->
        if length > best_length then (length, i)
        else (best_length, best_i)
      ) lengths (0, 0) in
    CCList.take best_length (CCList.drop best_i list)
end

let list_starts_with sub list =
  let sub_len = CCList.length sub in
  let prefix = CCList.take sub_len list in
  CCList.equal Stdlib.(=) prefix sub

let list_remove_sub_from_start sub list =
  if list_starts_with sub list then CCList.drop (CCList.length sub) list
  else list

let rec consume functions path =
  let consumed = CCList.fold_left
      (fun acc fn -> list_remove_sub_from_start fn acc) path functions in
  if CCList.equal Stdlib.(=) consumed path then path
  else consume functions consumed

let find_functions_of_lengths lengths path =
  let rec loop unused functions remaining_path =
    match unused with
    | [] -> CCList.rev functions
    | l::ls -> let consumed = consume functions remaining_path in
      let new_fn = CCList.take l consumed in
      loop ls (new_fn :: functions) consumed in
  loop (CCList.tl lengths) [CCList.take (CCList.hd lengths) path] path

let find_functions path =
  let gen () = Gen.int_range 4 12 in
  Gen.fold (fun fns a_len ->
      Gen.fold (fun fns b_len ->
          Gen.fold (fun fns c_len ->
              match fns with
              | Some fns -> Some fns
              | None ->
                let new_fns = find_functions_of_lengths [a_len; b_len; c_len] path in
                let consumed = consume new_fns path in
                if CCList.length consumed = 0 then Some new_fns else None
            )
            fns (gen ()))
        fns (gen ())) None (gen ())
  |> CCOpt.get_exn

let compress functions path =
  let rec loop compressed remaining_path =
    match remaining_path with
    | [] -> CCList.rev compressed
    | _ ->
      let compressed, remaining_path = CCList.foldi (fun (compressed, remaining_path) i fn ->
          if list_starts_with fn remaining_path then
            let new_remaining_path = list_remove_sub_from_start fn remaining_path in
            let fn_id = match i with
              | 0 -> "A"
              | 1 -> "B"
              | 2 -> "C"
              | _ -> failwith "Too many functions" in
            ((fn_id :: compressed), new_remaining_path)
          else (compressed, remaining_path)
        ) (compressed, remaining_path) functions in
      loop compressed remaining_path
  in
  CCString.concat "," (loop [] path)

let create_input main functions visualize =
  let fns_as_str = CCList.map Instruction.show_list functions in
  let program_input = CCList.flatten [[main]; fns_as_str] in
  let as_chars = CCList.map CCString.to_list program_input in
  let terminated = CCList.map (fun line -> line @ ['\n']) as_chars in
  let as_ints = CCList.map (CCList.map int_of_char) terminated in
  (CCList.flatten as_ints) @ [int_of_char visualize; int_of_char '\n']

let run_robot program input =
  let memory = program.memory in
  let altered_memory = memory_update 0 2 memory in
  let input_gen = Gen.of_list input in
  let output_handler = (fun output _state_opt program ->
      if output > 255 then Printf.printf "Dust collected %d\n" output;
      (None, program)) in
  let program = program_make ~input: input_gen ~output: output_handler altered_memory in
  program_execute program
      

let solve_b program =
  let maze = generate_maze program in
  let path = Scaffolding.find_path maze in
  let functions = find_functions path in
  let compressed = compress functions path in
  let input = create_input compressed functions 'n' in
  run_robot program input

let _ =
  let memory = read_memory() in
  let program = program_make memory in
  time (fun () -> solve program);
  time (fun () -> solve_b program)

