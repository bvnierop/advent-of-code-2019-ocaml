open Util
open IntCodeComputer

let solve input memory =
  program_execute ~verbose: false ~print_outputs: true
      (program_make ~input: (Gen.singleton input) memory) |> ignore

    
let _ =
  let memory = read_memory () in
  time (fun () -> solve 1 memory);
  time (fun () -> solve 5 memory)
