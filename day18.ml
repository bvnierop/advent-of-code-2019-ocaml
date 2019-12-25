open Util

module KeySet = CCSet.Make(Char)
let show_keyset keyset =
  KeySet.to_list keyset
  |> CCList.map (fun key -> Printf.sprintf "%c" key)
  |> CCString.concat ", "

  
module KeyMap = CCMap.Make(Char)

module Point = struct
  type t = (int * int)
  let compare = Stdlib.compare
  let x (x, _) = x
  let y (_, y) = y
  let zero = (0, 0)
  let _dx = [0; 1; 0; -1]
  let _dy = [-1; 0; 1; 0]
  let neighbours pt = CCList.map2 (fun dx dy -> let x, y = pt in (x + dx, y + dy)) _dx _dy

  let show pt = Printf.sprintf "(%d, %d)" (x pt) (y pt)
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

  let find_keys grid =
    CCArray.foldi (fun keymap y row ->
        CCArray.foldi (fun keymap x tile ->
            match tile with
            | Tile.Key id -> KeyMap.add id (x, y) keymap
            | _ -> keymap)
          keymap row)
      KeyMap.empty grid

  let read () =
    let grid = read_all read_line
               |> CCList.map (CCString.to_array)
               |> CCList.map (CCArray.map Tile.of_char)
               |> CCArray.of_list in
    { grid = grid;
      entrance = _find Tile.Entrance grid;
      keys = find_keys grid }

  let tile point maze =
    let row = CCArray.get maze.grid (Point.y point) in
    CCArray.get row (Point.x point)

  let passable point target_key collected_keys maze =
    match tile point maze with
    | Wall -> false
    | Passage -> true
    | Entrance -> true
    | Key k -> k = target_key || KeySet.mem k collected_keys
    | Door k -> let key = Char.lowercase_ascii k in
      KeySet.mem key collected_keys


  let show_keys maze =
    KeyMap.to_list maze.keys
    |> CCList.map (fun (key, (x, y)) ->
        Printf.sprintf "%c: (%d, %d)" key x y)
    |> CCList.rev
    |> CCString.concat ", "

  let keyset maze =
    KeySet.of_seq (KeyMap.keys maze.keys)

  let show maze =
    let grid_str = CCArray.map (CCArray.map Tile.to_char) maze.grid
                   |> CCArray.map CCString.of_array
                   |> CCArray.to_list
                   |> CCString.concat "\n" in
    let x, y = maze.entrance in
    Printf.sprintf "%s\nKeys: %s\nEntrance at: (%d, %d)" grid_str (show_keys maze) x y
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

  let with_key (key: char) node = { node with keys = KeySet.add key node.keys }
  let with_point (point: Point.t) node = { node with point = point }

  let add_key (key: char) (maze: Maze.t) (node: t) =
    with_point (KeyMap.find key maze.keys)  (with_key key node)

  let neighbours (maze: Maze.t) (node: t) =
    let available_keys = KeySet.diff (Maze.keyset maze) node.keys in
    KeySet.to_list available_keys
    |> CCList.map (fun key -> add_key key maze node)

  let show node =
    Printf.sprintf "[%s: %s]" (Point.show node.point) (show_keyset node.keys)
    
end

module Path = struct
  let length (node: Node.t) (neighbour: Node.t) maze =
    (* let _ = Printf.printf "Length from %s to %s\n" (Node.show node) (Node.show neighbour) in *)
    let module PointSet = CCSet.Make(Point) in
    let target_key = KeySet.min_elt (KeySet.diff neighbour.keys node.keys) in
    let is_seen pt all = PointSet.mem pt all in
    let seen pt all = PointSet.add pt all in
    let rec process queue points_seen =
      if CCFQueue.is_empty queue then
        None
      else begin
        let ((loc, distance), rest): ((Point.t * int) * (Point.t * int) CCFQueue.t) = CCFQueue.take_front_exn queue in
        (* let x, y = loc in
         * let _ = Printf.printf "Processing (%d, %d)\n" x y in *)
        if loc = neighbour.point then Some distance
        else begin
          let new_queue, points_seen = CCList.fold_left (fun (queue, points_seen) pt ->
              if Maze.passable pt target_key node.keys maze && not (is_seen pt points_seen) then
                (CCFQueue.snoc queue (pt, distance + 1),
                 seen pt points_seen)
              else (queue, points_seen)) (rest, points_seen) (Point.neighbours loc) in
          process new_queue points_seen
        end
      end in
    process (CCFQueue.of_list [(node.point, 0)]) (PointSet.of_list [node.point])
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

  module PathCache = struct
    module Key = struct
      type t = (Point.t * Point.t * KeySet.t)
      let compare a b =
        let src_a, dst_a, keys_a = a in
        let src_b, dst_b, keys_b = b in
        let cmp_src = Stdlib.compare src_a src_b in
        let cmp_dst = Stdlib.compare dst_a dst_b in
        let cmp_keys = KeySet.compare keys_a keys_b in
        if cmp_src = 0 then
          if cmp_dst = 0 then cmp_keys
          else cmp_dst
        else cmp_src
      let of_terms (src: Node.t) (dst: Node.t) = (src.point, dst.point, src.keys)

      let show (key: t) =
        let a, b, keys = key in
        Printf.sprintf "[%s %s: %s]" (Point.show a) (Point.show b) (show_keyset keys)
    end

    module CacheMap = CCMap.Make(Key)
    type t = int CacheMap.t

    let empty = CacheMap.empty

    let opt_of_length l = match l with -1 -> None | n -> Some n
    let length_of_opt o = match o with None -> -1 | Some n -> n

    let get src dst maze cache =
      (Path.length src dst maze, cache)
      (* let cache_key = Key.of_terms src dst in
       * match CacheMap.get cache_key cache with
       * | Some len ->
       *   (opt_of_length len, cache)
       * | None -> let length_opt = Path.length src dst maze in
       *   (length_opt, CacheMap.add cache_key (length_of_opt length_opt) cache) *)

    let show cache =
      CacheMap.to_list cache
      |> CCList.rev
      |> CCList.filter (fun ((_, _, keys), _) -> KeySet.cardinal keys = 2)
      |> CCList.map (fun (key, len) ->
          Printf.sprintf "%s -> %d"  (Key.show key) len)
      |> CCString.concat "\n"

    let size cache =
      CacheMap.cardinal cache
  end

  module PQ = CCHeap.Make_from_compare(Elt)

  module Distances = struct
    module NodeMap = CCMap.Make(Node)
    type t = int NodeMap.t
    let empty = NodeMap.empty
    let get node dists = NodeMap.get_or ~default: max_int node dists
    let update node distance dists = NodeMap.add node distance dists
  end

  module Handled = struct
    module NodeSet = CCSet.Make(Node)
    type t = NodeSet.t
    let empty = NodeSet.empty
    let get node set = NodeSet.mem node set
    let set node set = NodeSet.add node set
  end

  let perform src_node dst_fn maze =
    let pq = PQ.add PQ.empty (Elt.of_node src_node) in
    let rec process pq dists handled path_cache =
      let pq, (distance, node) = PQ.take_exn pq in
      let _ = Printf.printf "Looking at %s: %d\n" (Node.show node) distance in
      if dst_fn node then
        (distance, node)
      else if not (Handled.get node handled) then (* If it's longer, we're not interested. The node is outdated *)
        let handled = Handled.set node handled in
        let updated_queue, updated_dists, updated_path_cache =
          CCList.fold_left (fun (pq, dists, path_cache) neighbour ->
              let path_length, updated_path_cache = PathCache.get node neighbour maze path_cache in (* Path.length node neighbour maze in *)
              match path_length with
              | None -> (pq, dists, updated_path_cache)
              | Some dist -> let dist_to_neighbour = distance + dist in
                if  dist_to_neighbour < Distances.get neighbour dists then
                  let updated_distances = Distances.update neighbour dist_to_neighbour dists in
                  let updated_queue = PQ.add pq (Elt.make dist_to_neighbour neighbour) in
                  (* let _ = Printf.printf "  Enqueue %s: %d\n" (Node.show neighbour) dist_to_neighbour in *)
                  (updated_queue, updated_distances, updated_path_cache)
                else (pq, dists, updated_path_cache)) (pq, dists, path_cache) (Node.neighbours maze node) in
        process updated_queue updated_dists handled updated_path_cache
      else begin
        (* print_endline "  Discard..."; *)
        process pq dists handled path_cache
      end
    in
    process pq (Distances.update src_node 0 Distances.empty) Handled.empty PathCache.empty
end

let solve_a () =
  let maze = Maze.read () in
  print_endline (Maze.show maze);
  let start = Node.with_point maze.entrance Node.empty in
  let end_fn (node: Node.t) = KeySet.equal (Maze.keyset maze) node.keys in
  let distance, node = Dijkstra.perform start end_fn maze in
  Printf.printf "Total steps: %d\nFinal node: %s\n" distance (Node.show node)

let _ =
  time solve_a
