open Util
open IntCodeComputer

module Point = struct
  type t = (int * int) [@@deriving show]
  let compare = Stdlib.compare

  let moved direction (x, y) =
    match direction with
    | 1 -> (x, y - 1)
    | 2 -> (x, y + 1)
    | 3 -> (x - 1, y)
    | 4 -> (x + 1, y)
    | _ -> failwith "Invalid direction"

  let neighbours point =
    CCList.map (fun dir -> (dir, moved dir point)) [1; 2; 3; 4]
end

module PointMap = CCMap.Make(Point)
module PointSet = CCSet.Make(Point)

module Seen = struct
  type t = PointSet.t
  let is_seen point seen = PointSet.mem point seen
  let seen point seen = PointSet.add point seen
  let empty = PointSet.empty
end

module Tile = struct
  type t =
    | Wall
    | Open
    | Oxygen

  let of_int i =
    match i with
    | 0 -> Wall
    | 1 -> Open
    | 2 -> Oxygen
    | _ -> failwith "Invalid tile"

  let to_string tile =
    match tile with
    | Wall -> "#"
    | Open -> "."
    | Oxygen -> "@"
end

let solve_step queue seen =
  let (current, tile, distance, program) = CCFQueue.first_exn queue in
  let queue = CCFQueue.tail queue in
  match tile with
  | Tile.Oxygen -> print_endline (string_of_int distance); (CCFQueue.empty, seen)
  | Wall -> failwith "Cannot dequeue a wall"
  | Open -> let neighbours = Point.neighbours current in
    CCList.fold_left (fun (queue, seen) (direction, neighbour) ->
        if Seen.is_seen neighbour seen then (queue, seen)
        else
          let next_program = program_execute
              (program_with_input_gen (Gen.singleton direction) program) in
          let tile = Tile.of_int (program_last_output next_program) in
          let next_seen = Seen.seen neighbour seen in
          match tile with
          | Open
          | Oxygen -> (CCFQueue.snoc queue (neighbour, tile, distance + 1, next_program),
                       next_seen)
          | Wall -> (queue, next_seen)) (queue, seen) neighbours

let solve memory =
  let program = program_make memory in
  let queue = CCFQueue.snoc CCFQueue.empty ((0, 0), Tile.Open, 0, program) in
  let seen = Seen.empty in
  let rec search queue seen =
    let queue, seen = solve_step queue seen in
    if not (CCFQueue.is_empty queue) then search queue seen in
  search queue seen

let _ =
  let memory = read_memory () in
  time (fun () -> solve memory)
