open Util

(* Graph things *)
module NodeMap = CCMap.Make(String) [@@deriving show]
module NodeSet = CCSet.Make(String) [@@deriving show]
type node = string

type graph = {
  children: node list NodeMap.t;
  parents: node NodeMap.t;
  nodes: NodeSet.t
}

let graph_empty = {
  children = NodeMap.empty;
  parents = NodeMap.empty;
  nodes = NodeSet.empty
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

let graph_foldi ~fn ~initial graph =
  let roots = graph_roots graph in
  let rec dive accumulator index node =
    (* Printf.printf "dive ? %d %s\n" index node; *)
    let next = fn accumulator index node in
    let children = NodeMap.get_or ~default: [] node graph.children in
    CCList.fold_left (fun acc child ->
        dive acc (index + 1) child) next children
  in
  CCList.fold_left (fun acc root -> dive acc 0 root) initial roots

(* Solving the problem *)
let read_orbit_graph () =
  read_all_lines ()
  |> CCList.map (fun line ->
      let splitted = CCString.split_on_char ')' line in
      let (fst, tl) = CCList.hd_tl splitted in
      (fst, CCList.hd tl))
  |> CCList.fold_left (fun acc (src, dst) ->
      graph_add_edge src dst acc) graph_empty

let solve graph =
  graph_foldi ~fn: (fun acc idx _n -> acc + idx)
    ~initial: 0
    graph
    |> string_of_int
    |> print_endline
    
let _ =
  let graph = read_orbit_graph () in
  time (fun () -> solve graph)
