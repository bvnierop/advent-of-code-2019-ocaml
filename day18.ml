open Util

module KeySet = CCSet.Make(Char)
module KeyMap = CCMap.Make(Char)

module Point = struct
  type t = (int * int)
  let x (x, _) = x
  let y (_, y) = y
  let zero = (0, 0)
end

module Tile = struct
  type t = Passage | Wall | Door of char | Key of char | Entrance

  let _parse_char chr =
    if chr >= 'a' && chr <= 'z' then Key chr
    else if chr >= 'A' && chr <= 'Z' then Door chr
    else failwith (Printf.sprintf "Unknown tile: %c" chr)

  let of_char = function 
    | '.' -> Passage
    | '#' -> Wall
    | '@' -> Entrance
    | chr -> _parse_char chr

  let to_char = function
    | Passage -> '.'
    | Wall -> '#'
    | Entrance -> '@'
    | Key id -> id
    | Door id -> id

  let show tile = CCString.of_char (to_char tile)
end

module Maze = struct
  type t = {
    grid: Tile.t CCArray.t CCArray.t;
    entrance: Point.t;
    keys: Point.t KeyMap.t;
  }

  let _find needle grid =
    CCArray.foldi (fun pt y row ->
        CCArray.foldi (fun pt x tile -> if tile = needle then (x, y) else pt)
          pt row)
      Point.zero grid

  let read () =
    let grid = read_all read_line
               |> CCList.map (CCString.to_array)
               |> CCList.map (CCArray.map Tile.of_char)
               |> CCArray.of_list in
    { grid = grid;
      entrance = _find Tile.Entrance grid;
      keys = KeyMap.empty }

  let show_keys maze =
    KeyMap.to_list maze.keys
    |> CCList.map (fun (key, (x, y)) ->
        Printf.sprintf "%c: (%d, %d)" key x y)
    |> CCString.concat ", "

  let show maze =
    let grid_str = CCArray.map (CCArray.map Tile.to_char) maze.grid
                   |> CCArray.map CCString.of_array
                   |> CCArray.to_list
                   |> CCString.concat "\n" in
    let x, y = maze.entrance in
    Printf.sprintf "%s\nEntrance at: (%d, %d)\n" grid_str x y
end

module Node = struct
  type t = {
    point: Point.t;
    keys: KeySet.t;
  }

  let equal a b = a.point = b.point && KeySet.equal a.keys b.keys

  let compare a b =
    let pt_cmp = Stdlib.compare a.point b.point in
    if pt_cmp = 0 then KeySet.compare a.keys b.keys
    else pt_cmp

  let empty = { point = Point.zero; keys = KeySet.empty }

  let of_point point = { point = point; keys = KeySet.empty }

  let with_key (key: char) node =
    { node with keys = KeySet.add key node.keys }

  let neighbours maze node =
    ignore maze; ignore node;
    failwith "Not implemented: Node.neighbours"
end

module Path = struct
  let length node neighbour maze =
    ignore node; ignore neighbour; ignore maze;
    failwith "Not implemented: Path.length"
end

module Dijkstra = struct
  module Elt = struct
    type t = (int * Node.t)

    let compare (dist_a, node_a) (dist_b, node_b) =
      let dist_compare = Stdlib.compare dist_a dist_b in
      if dist_compare = 0 then Node.compare node_a node_b
      else dist_compare

    let empty = (0, Node.empty)

    let of_node node = (0, node)

    let make distance node = (distance, node)
  end

  module PQ = CCHeap.Make_from_compare(Elt)

  module Distances = struct
    module NodeMap = CCMap.Make(Node)
    type t = int NodeMap.t
    let empty = NodeMap.empty
    let get node dists = NodeMap.get_or ~default: max_int node dists
    let update node distance dists = NodeMap.add node distance dists
  end

  let perform src_node dst_fn maze =
    ignore dst_fn;
    let pq = PQ.add PQ.empty (Elt.of_node src_node) in
    let rec process pq dists =
      let pq, (distance, node) = PQ.take_exn pq in
      if dst_fn node then (distance, node)
      else begin
        let updated_queue, updated_dists =
          CCList.fold_left (fun (pq, dists) neighbour ->
              let dist_to_neighbour = distance + Path.length node neighbour maze in
              if  dist_to_neighbour < Distances.get neighbour dists then
                let updated_distances =
                  Distances.update neighbour dist_to_neighbour dists in
                let updated_queue = PQ.add pq (Elt.make dist_to_neighbour neighbour) in
                (updated_queue, updated_distances)
              else (pq, dists)) (pq, dists) (Node.neighbours maze node) in
        process updated_queue updated_dists
      end
    in
    process pq Distances.empty
end

let solve_a () =
  let maze = Maze.read () in
  print_endline (Maze.show maze)
(*
Steps to solve
- read input, store it in a grid (pref 2d array)
- think of a way to represent a node
  > each node needs: 1) x/y 2) collected keys.
  > while bitflags would be nice, I suspect a record of point + set with keys will do
- Run Dijkstra with source being the starting point and
  all keys being the target
  > Neighbours are all unreached keys
  > Do BFS for distances
*)

let _ =
  time solve_a
