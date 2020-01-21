open Util

module KeySet = struct
  type t = int

  let empty = 0

  let (<<) a b = a lsl b
  let (>>) a b = a lsr b

  let of_char key =
    let offset = int_of_char key - 97 (* 'a' *) in
    1 << offset

  let add key set =
    set lor (of_char key)

  let of_list list =
    CCList.fold_left (fun set key -> add key set) empty list

  let to_list set =
    let rec iter rem idx acc =
      if rem = 0 then CCList.rev acc
      else if rem land 0x1 = 0x1 then iter (rem >> 1) (idx + 1) ((char_of_int (idx + 97)) :: acc)
      else iter (rem >> 1) (idx + 1) acc
    in
    iter set 0 []

  let of_seq seq =
    CCList.of_seq seq |> of_list

  let mem key set =
    let of_key = of_char key in
    of_key land set != 0

  let equal = Stdlib.(=)
  let compare = Stdlib.compare


  let diff a b =
    let mask = b land a in
    a lxor mask

  let min_elt set =
    let rec find idx set =
      if set land 0x1 = 0x1 then idx
      else find (idx + 1) (set >> 1)
    in
    (char_of_int (find 0 set + 97))

  let cardinal set =
    let rec count num set =
      if set = 0 then 0
      else if set land 0x1 = 0x1 then count (num + 1) (set >> 1)
      else count num (set >> 1)
    in
    count 0 set
end

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

  let key_char_of_door_char d =
    char_of_int ((int_of_char d) - 65 + 97)

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

module Path2 = struct
  module Cache = struct
    module Key = struct
      type t = (Point.t * Point.t)
      let compare = Stdlib.compare
    end

    module Value = struct
      type t = {
        distance: int;
        required_keys: KeySet.t;
      }

      let compare = Stdlib.compare
    end

    module CacheMap = CCMap.Make(Key)

    type t = Value.t list CacheMap.t

    let empty = CacheMap.empty

    let add_path src dst distance required_keys cache =
      CacheMap.update (src, dst) (function
          | None -> Some [{ Value.distance = distance; required_keys = required_keys }]
          | Some paths ->
            let new_paths = { Value.distance = distance;
                              required_keys = required_keys } :: paths in
            Some (CCList.sort Value.compare new_paths))
        cache
  end

  let find_paths_from_one_key_to_all_others src_loc (maze: Maze.t) =
    let module Seen = CCSet.Make(struct
        type t = (Point.t * KeySet.t)
        let compare a b =
          let pa, ka = a in
          let pb, kb = b in
          let cmp_p = Point.compare pa pb in
          let cmp_k = KeySet.compare ka kb in
          if cmp_p = 0 then cmp_k else cmp_p
      end) in
    let rec bfs queue seen paths =
      if CCFQueue.is_empty queue then
        paths
      else begin
        let (loc, distance, doors, keys, path), rest = CCFQueue.take_front_exn queue in
        let updated_paths = match Maze.tile loc maze with
          | Tile.Key _ -> (src_loc, loc, distance, doors, keys) :: paths
          | _ -> paths in
        let updated_queue, updated_seen = CCList.fold_left (fun (q, s) n ->
            if not (Seen.mem (n, doors) s) then
              match Maze.tile n maze with
              | Entrance
              | Passage -> (CCFQueue.snoc q (n, distance + 1, doors, keys, n :: path), Seen.add (n, doors) s)
              | Key k -> (CCFQueue.snoc q (n, distance + 1, doors, KeySet.add k keys, n :: path), Seen.add (n, doors) s)
              | Door d -> let new_doors = KeySet.add (Tile.key_char_of_door_char d) doors in
                let backfilled_seen = CCList.fold_left (fun seen point ->
                    Seen.add (point, new_doors) seen) s path in
                (CCFQueue.snoc q (n, distance + 1, new_doors, keys, n :: path), Seen.add (n, new_doors) backfilled_seen)
              | Wall -> (q, s)
            else (q, s)
          ) (rest, seen) (Point.neighbours loc) in
        bfs updated_queue updated_seen updated_paths
      end
    in
    bfs (CCFQueue.snoc CCFQueue.empty (src_loc, 0, KeySet.empty, KeySet.empty, [])) (Seen.add (src_loc, KeySet.empty) Seen.empty) []


  let build_cache (maze: Maze.t) =
    Maze.keyset maze
    |> KeySet.to_list
    |> CCList.map (fun key -> KeyMap.find key maze.keys)
    |> (fun list -> maze.entrance :: list)
    |> CCList.fold_left (fun paths loc ->
        paths @ find_paths_from_one_key_to_all_others loc maze) []
    |> CCList.fold_left
      (fun cache (src_loc, dst_loc, distance, doors, _keys) ->
         Cache.add_path src_loc dst_loc distance doors cache) Cache.empty

  let length src dst have_keys (cache: Cache.t) =
    let paths = Cache.CacheMap.get_or ~default: [] (src, dst) cache in
    CCList.fold_left (fun dist (path: Cache.Value.t) ->
        if KeySet.diff path.required_keys have_keys = KeySet.empty then
          Some (match dist with None -> path.distance | Some d -> min d path.distance)
        else
          dist) None paths

  let show_cache cache =
    let show_keys keys = "[" ^ (KeySet.to_list keys |> CCList.map (String.make 1) |> String.concat ",") ^ "]" in
    let show_path src dst distance doors =
      Printf.sprintf "From %s to %s takes %d steps, requires %s keys."
        (Point.show src) (Point.show dst) distance (show_keys doors)
    in
    let show_for_key key =
      let src, dst = key in
      let paths = Cache.CacheMap.get_or ~default: [] key cache in
      CCList.map (fun (p: Cache.Value.t) -> show_path src dst p.distance p.required_keys) paths
      |> CCString.concat "\n" in
    let keys = Cache.CacheMap.keys cache |> CCList.of_seq in
    CCList.map show_for_key keys
    |> String.concat "\n"
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

  module Handled = struct
    module NodeSet = CCSet.Make(Node)
    type t = NodeSet.t
    let empty = NodeSet.empty
    let get node set = NodeSet.mem node set
    let set node set = NodeSet.add node set
  end

  let perform src_node dst_fn maze =
    let path_cache = Path2.build_cache maze in
    let pq = PQ.add PQ.empty (Elt.of_node src_node) in
    let rec process pq dists handled =
      let pq, (distance, node) = PQ.take_exn pq in
      (* let _ = Printf.printf "Looking at %s: %d\n" (Node.show node) distance in *)
      if dst_fn node then
        (distance, node)
      else if not (Handled.get node handled) then (* If it's longer, we're not interested. The node is outdated *)
        let handled = Handled.set node handled in
        let updated_queue, updated_dists =
          CCList.fold_left (fun (pq, dists) (neighbour: Node.t) ->
              (* let path_length, updated_path_cache = PathCache.get node neighbour maze path_cache in (\* Path.length node neighbour maze in *\) *)
              let path_length = Path2.length node.point neighbour.point node.keys path_cache in
              match path_length with
              | None -> (pq, dists)
              | Some dist -> let dist_to_neighbour = distance + dist in
                if  dist_to_neighbour < Distances.get neighbour dists then
                  let updated_distances = Distances.update neighbour dist_to_neighbour dists in
                  let updated_queue = PQ.add pq (Elt.make dist_to_neighbour neighbour) in
                  (* let _ = Printf.printf "  Enqueue %s: %d\n" (Node.show neighbour) dist_to_neighbour in *)
                  (updated_queue, updated_distances)
                else (pq, dists)) (pq, dists) (Node.neighbours maze node) in
        process updated_queue updated_dists handled
      else begin
        (* print_endline "  Discard..."; *)
        process pq dists handled
      end
    in
    process pq (Distances.update src_node 0 Distances.empty) Handled.empty
end

let solve_a (maze: Maze.t) () =
  let start = Node.with_point maze.entrance Node.empty in
  let end_fn (node: Node.t) = KeySet.equal (Maze.keyset maze) node.keys in
  let distance, node = Dijkstra.perform start end_fn maze in
  Printf.printf "Total steps: %d\nFinal node: %s\n" distance (Node.show node)

let _ =
  let maze = Maze.read() in
  print_endline (Maze.show maze);
  time (solve_a maze)
