open Util
open IntCodeComputer

let solve input memory =
  let program = program_make ~input: (Gen.singleton input) memory in
  let executed = program_execute ~verbose: false ~print_outputs: true program in
  ignore executed

let _ =
  let memory = read_memory () in
  time (fun () -> solve 1 memory);
  time (fun () -> solve 2 memory)
