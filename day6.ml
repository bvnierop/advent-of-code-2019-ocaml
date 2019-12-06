open Util

(* Graph things *)
module NodeMap = CCMap.Make(String) [@@deriving show]
module NodeSet = CCSet.Make(String) [@@deriving show]
type node = string

type graph = {
  children: node list NodeMap.t;
  parents: node NodeMap.t;
  nodes: NodeSet.t;
}

let graph_empty = {
  children = NodeMap.empty;
  parents = NodeMap.empty;
  nodes = NodeSet.empty;
}

let graph_add_edge (src: node) (dst: node) (graph: graph) =
  let new_parents = NodeMap.add dst src graph.parents in
  let current_children = NodeMap.get_or ~default: [] src graph.children in
  let new_children = NodeMap.add src (dst :: current_children) graph.children in
  let new_nodes = NodeSet.add src graph.nodes |> NodeSet.add dst in
  { parents = new_parents; children = new_children; nodes = new_nodes }

let graph_roots graph =
    NodeSet.filter (fun n -> not (NodeMap.mem n graph.parents)) graph.nodes
    |> NodeSet.to_list

let graph_neighbours node graph =
  let children = NodeMap.get_or node ~default: [] graph.children in
  match NodeMap.get node graph.parents with
  | None -> children
  | Some p -> p :: children

let graph_foldi ~fn ~initial ~start graph =
  (* let roots = graph_roots graph in *)
  let is_seen node seen = NodeSet.mem node seen in
  let set_seen node seen = NodeSet.add node seen  in
  let rec dive (acc, seen) index node =
    let next_acc = fn acc index node in
    let next_seen = set_seen node seen in
    let neighbours = graph_neighbours node graph in
    CCList.fold_left (fun (acc, seen) child ->
        if not (is_seen child seen) then
          dive (acc, seen) (index + 1) child
        else (acc, seen)) (next_acc, next_seen) neighbours
  in
  dive (initial, NodeSet.empty) 0 start
  |> (fun (acc, _seen) -> acc)
  

(* Solving the problem *)
let read_orbit_graph () =
  read_all_lines ()
  |> CCList.map (fun line ->
      let splitted = CCString.split_on_char ')' line in
      let (fst, tl) = CCList.hd_tl splitted in
      (fst, CCList.hd tl))
  |> CCList.fold_left (fun acc (src, dst) ->
      graph_add_edge src dst acc) graph_empty

let solve_a graph =
  graph_roots graph
  |> CCList.fold_left (fun sum node ->
      graph_foldi ~fn: (fun acc idx _n -> acc + idx)
        ~initial: sum ~start: node graph) 0
  |> string_of_int
  |> print_endline

let solve_b graph =
  graph_foldi ~fn: (fun dst idx node ->
      if node = "SAN" then idx - 2
      else dst)
    ~initial: 0 ~start: "YOU" graph
  |> string_of_int
  |> print_endline

let _ =
  let graph = read_orbit_graph () in
  time (fun () -> solve_a graph);
  time (fun () -> solve_b graph)
