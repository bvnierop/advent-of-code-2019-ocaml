open Util

module Reaction = struct
  type chemical = string [@@deriving show]
  type input = (chemical * int) [@@deriving show]
  type output = (chemical * int) [@@deriving show]
  type t = {
    target: output;
    inputs: input list;
  } [@@deriving show]

  module StringMSet = CCMultiSet.Make(String) 
  module StringMap = CCMap.Make(String)
  type lookup_entry = (int * StringMSet.t) 
  type lookup = lookup_entry StringMap.t

  let empty_bag = StringMSet.empty
  let fuel_bag = StringMSet.add StringMSet.empty "FUEL"
  let multiply_bag times bag =
    StringMSet.to_list_mult bag
    |> CCList.map (fun (chem, count) -> (chem, count * times))
    |> StringMSet.of_list_mult

  let make_lookup_entry reaction =
    let chem, amount = reaction.target in
    let req = StringMSet.of_list_mult reaction.inputs in
    (chem, (amount, req))

  let make_lookup reactions =
    CCList.map make_lookup_entry reactions
    |> StringMap.of_list

  let show_set set =
    StringMSet.to_list_mult set
    |> CCList.map show_input
    |> CCString.concat ", "
    |> Printf.sprintf "M[ %s ]"

  let show_lookup_entry entry =
    let chem, (count, set) = entry in
    Printf.sprintf "%d %s: %s" count chem (show_set set)

  let show_lookup lookup =
    StringMap.to_list lookup
    |> CCList.map show_lookup_entry
    |> CCString.concat "\n"

  let pp_lookup out lookup =
    Format.fprintf out "%s" (show_lookup lookup)

  let make target inputs =
    { target = target; inputs = inputs }

  let parse_input input =
    CCString.trim input
    |> (fun trimmed -> Scanf.sscanf trimmed "%d %s" (fun amt name -> (name, amt)))

  let parse_inputs inputs =
    CCString.split ~by: "," inputs
    |> CCList.map parse_input

  let parse_line line =
    CCString.split ~by: "=>" line
    |> (fun l ->
        match l with
        | inputs::target::_ -> make (parse_input target) (parse_inputs inputs)
        | _ -> failwith "Unexpected input")

  let read () = read_line () |> parse_line


  (* Solve *)
  let rec ore_needed ~have ~goal lookup ore =
    if StringMSet.is_empty goal then ore
    else let chem = StringMSet.min goal in
      let next_goal = (StringMSet.remove_all goal chem) in
      if chem = "ORE" then
        ore_needed ~have: have
          ~goal: next_goal
          lookup
          ((StringMSet.count goal chem) + ore)
      else (* not ore *)
        let amount_made, inputs = StringMap.find chem lookup in
        let wanted = StringMSet.count goal chem in
        let times, surplus = div_ceil wanted  amount_made in
        let required = multiply_bag times inputs in
        let new_goal = StringMSet.union required next_goal in
        let new_have = StringMSet.add_mult have chem (abs surplus) in
        let common = StringMSet.intersection new_goal new_have in
        ore_needed
          ~have: (StringMSet.diff new_have common)
          ~goal: (StringMSet.diff new_goal common)
          lookup
          ore

end


let read_reactions () =
  read_all Reaction.read

let solve reactions =
  let lookup = Reaction.make_lookup reactions in
  Reaction.ore_needed ~have: Reaction.empty_bag
    ~goal: Reaction.fuel_bag
    lookup 0
  |> string_of_int |> print_endline

let _ =
  let reactions = read_reactions () in
  time (fun () -> solve reactions)
