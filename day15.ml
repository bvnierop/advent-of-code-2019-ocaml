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

let solve_step queue seen predicate =
  let (current, tile, distance, program) = CCFQueue.first_exn queue in
  let queue = CCFQueue.tail queue in
  if predicate current tile then (CCFQueue.empty, seen, current, distance, program)
  else let neighbours = Point.neighbours current in
    let final_queue, final_seen =
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
            | Wall -> (queue, next_seen)) (queue, seen) neighbours in
    (final_queue, final_seen, current, distance, program)


let bfs point predicate program =
  let queue = CCFQueue.snoc CCFQueue.empty (point, Tile.Open, 0, program) in
  let seen = Seen.seen point Seen.empty in
  let rec search queue seen =
    let queue, seen, last, distance, prog = solve_step queue seen predicate in
    if CCFQueue.is_empty queue then (last, distance, prog)
    else search queue seen in
  search queue seen

let find_oxygen program =
  let predicate = (fun _ tile -> match tile with
      | Tile.Oxygen -> true
      | Open -> false
      | Wall -> failwith "Walls should not be queued") in
  bfs (0, 0) predicate program

let fill_oxygen point program =
  bfs point (fun _ _ -> false) program

let solve memory =
  let program = program_make memory in
  let (point, distance, _) = find_oxygen program in
  Printf.printf "Oxygen system found at %s, %d steps away!\n" (Point.show point) distance

let solve_b memory =
  let program = program_make memory in
  let (point, _, program) = find_oxygen program in
  let (_, distance, _) = fill_oxygen point program in
  Printf.printf "Room filled after %d minutes\n" distance

let _ =
  let memory = read_memory () in
  time (fun () -> solve memory);
  time (fun () -> solve_b  memory)
