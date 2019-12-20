open Util
open IntCodeComputer

module Tile = struct
  type t =
    | Scaffold
    | Space
    | Robot

  let of_int i =
    match char_of_int i with
    | '#' -> Scaffold
    | '.' -> Space
    | '^' | 'v' | '<' | '>'  -> Robot
    | c -> failwith (Printf.sprintf "Invalid tile: %c" c)

  let to_string tile =
    match tile with
    | Scaffold -> "#"
    | Space -> "."
    | Robot -> "R"

  let pp out tile =
    Format.pp_print_string out (to_string tile)
end

module Grid = struct
  type row = Tile.t CCPersistentArray.t [@@deriving show]
  type t = row CCPersistentArray.t [@@deriving show]

  let empty: t = CCPersistentArray.make 1 (CCPersistentArray.make 0 Tile.Space)

  let get_row row_idx grid = CCPersistentArray.get grid row_idx
  let get_tile row_idx column_idx grid = CCPersistentArray.get (get_row row_idx grid) column_idx

  let last_row_index grid = (CCPersistentArray.length grid - 1)
  let set_row row_idx row grid : t = CCPersistentArray.set grid row_idx row

  let row_count grid = CCPersistentArray.length grid
  let col_count grid = CCPersistentArray.length (get_row 0 grid)

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


  let row_to_string (row: row) : string = CCPersistentArray.fold_left (fun str tile -> str ^ (Tile.to_string tile)) "> " row
  let to_string (grid: t) : string = CCPersistentArray.fold_left (fun str row -> str ^ "\n" ^ (row_to_string row)) "" grid
end

let test_grid = [
  46;46;35;46;46;46;46;46;46;46;46;46;46;10;
  46;46;35;46;46;46;46;46;46;46;46;46;46;10;
  35;35;35;35;35;35;35;46;46;46;35;35;35;10;
  35;46;35;46;46;46;35;46;46;46;35;46;35;10;
  35;35;35;35;35;35;35;35;35;35;35;35;35;10;
  46;46;35;46;46;46;35;46;46;46;35;46;46;10;
  46;46;35;35;35;35;35;46;46;46;35;46;46;10;
  (* 46; 35; 46; 10;
   * 35; 35; 35; 10;
   * 46; 35; 46; 10;
   * 46; 35; 35; *)
]

let program_return x program = (Some x, program)

let generate_maze program =
  let output_handler = (fun output grid_opt program ->
      match grid_opt with
      | None -> failwith "Set a state"
      | Some grid -> program_return (Grid.add_input output grid) program) in
  let prog = program_with_output ~initial: (Some Grid.empty) output_handler program in
  let executed = program_execute prog in
  CCOpt.get_exn executed.state |> Grid.trim

let test_maze =
  CCList.fold_left (fun grid tile -> Grid.add_input tile grid) Grid.empty test_grid |> Grid.trim

let solve program =
  let maze = generate_maze program in
  print_endline (Grid.to_string maze);
  Grid.fold (fun acc row col tile ->
      if row = 0 || col = 0 || row = Grid.row_count maze - 1 || col = Grid.col_count maze - 1 then acc
      else let north = Grid.get_tile (row - 1) col maze in
        let south = Grid.get_tile (row + 1) col maze in
        let east = Grid.get_tile row (col + 1) maze in
        let west = Grid.get_tile row (col - 1) maze in
        let all_the_same = CCList.uniq ~eq: Stdlib.(=) [tile; north; south; east; west;] |> CCList.length |> (fun l -> l = 1) in
        if tile = Tile.Scaffold && all_the_same then acc + (row * col)
        else acc) 0 maze
  |> string_of_int |> print_endline


let _ =
  let memory = read_memory() in
  let program = program_make memory in
  time (fun () -> solve program);
