open IntCodeComputer

let solve_a memory =
  let faulty_memory = memory_update 2L 2L (memory_update 1L 12L memory) in
  let final_state = program_execute ~verbose: false (program_make faulty_memory) in
  print_endline (Printf.sprintf "Final value of pos 0: %Ld" (program_output final_state))

let range a b =
  CCList.range a b |> CCList.map Int64.of_int

let solve_b memory =
  let options = CCList.product (fun a b -> (a, b)) (range 0 99) (range 0 99) in
  let target = 19690720L in
  let make_memory_attempt noun verb = memory_update 2L verb (memory_update 1L noun memory) in
  let (noun, verb) = CCList.find_pred_exn (fun (noun, verb) ->
      let mem = make_memory_attempt noun verb in
      let prog = program_make mem in
      let final_state = program_execute prog in
      target = (program_output final_state))
    options in
    print_endline (Printf.sprintf "100 * noun + verb = %Ld"
                     (Int64.mul 100L (Int64.add noun verb)))
    
let _ =
  let memory = read_memory () in
  solve_a memory;
  solve_b memory
