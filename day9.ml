open Util
open IntCodeComputer

let solve input memory =
  let program = program_make ~input: (Gen.singleton input) memory in
  let with_output = program_with_output (fun output state prog -> print_endline (string_of_int output); (state, prog)) program in
  let executed = program_execute ~verbose: false ~print_outputs: true with_output in
  ignore executed

let _ =
  let memory = read_memory () in
  time (fun () -> solve 1 memory);
  time (fun () -> solve 2 memory)
