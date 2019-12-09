open Util
open IntCodeComputer

let all_possible_phases =
  let phases = [0;1;2;3;4] in
  let phases_per_amp = [phases;phases;phases;phases;phases] in
  CCList.cartesian_product phases_per_amp
  |> CCList.filter (fun settings ->
      let uniq = CCList.uniq ~eq: (fun a b -> a = b) settings |> CCList.length in
      uniq = 5)

let solve_one phases memory =
  CCList.fold_left (fun outputs phase ->
      let gen = Gen.of_list (phase :: outputs) in
      let program = program_make ~input: gen memory in
      let executed = program_execute program in
      executed.outputs
    ) [0] phases
  |> CCList.hd

let solve memory =
  CCList.fold_left (fun hi trial ->
      max (solve_one trial memory) hi)
    0 all_possible_phases
  |> string_of_int |> print_endline

let _ =
  let memory = read_memory () in
  time (fun () -> solve memory)
