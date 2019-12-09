open Util
open IntCodeComputer

let phase_combinations_from phases =
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

let solve_two phases memory =
  let programs = CCList.map (fun p -> program_make ~input: (Gen.singleton p) memory) phases |> CCList.map program_execute in
  let queue = CCFQueue.of_list programs in
  let rec run q prev_output =
    if CCFQueue.is_empty q then
      prev_output
    else let program = CCFQueue.first_exn q in
      let with_input = program_with_input_gen (Gen.singleton prev_output) program in
      let executed = program_execute with_input in
      let output = program_last_output executed in
      match executed.run_state with
      | Terminated -> run (CCFQueue.tail q) output
      | WaitingForInput -> run (CCFQueue.snoc (CCFQueue.tail q) executed) output
      | Running -> failwith "At this stage, program should not be 'running'"
  in
  run queue 0

let solve phases memory solver =
  CCList.fold_left (fun hi trial ->
      max (solver trial memory) hi)
    0 (phase_combinations_from phases)
  |> string_of_int |> print_endline


let _ =
  let memory = read_memory () in
  time (fun () -> solve [0;1;2;3;4] memory solve_one);
  time (fun () -> solve [5;6;7;8;9] memory solve_two)
