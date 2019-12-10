open Util
open IntCodeComputer

let solve memory =
  let program = program_make ~input: (Gen.singleton 1L) memory in
  let executed = program_execute ~verbose: true ~print_outputs: true program in
  ignore executed

let _ =
  let memory = read_memory () in
  time (fun () -> solve memory)
