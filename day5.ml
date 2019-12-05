open Util
open IntCodeComputer

let solve_a memory =
  let _ = program_execute ~verbose: false ~print_outputs: true
      (program_make ~inputs: [1] memory) in
  1 |> ignore
    
let _ =
  let memory = read_memory () in
  time (fun () -> solve_a memory)
