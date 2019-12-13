open Util
open IntCodeComputer

let simulate memory =
  let output_handler = (fun output state program ->
      match state with
      | None -> failwith "No state given"
      | Some (acc, count) -> let built = output :: acc in
        if CCList.length built = 3 then begin
          if output = 2 then
            (Some ([], count + 1), program)
          else
            (Some ([], count), program)
        end
        else
          (Some (built, count), program)) in
  let program = program_with_output output_handler (program_make ~input: (Gen.empty) memory)
      ~initial: (Some ([], 0)) in
  let executed = program_execute ~verbose: false ~print_outputs: false program in
  match executed.state with
  | None -> 0
  | Some (_, count) -> count

let solve memory =
  simulate memory
  |> string_of_int
  |> print_endline


let _ =
  let memory = read_memory () in
  time (fun () -> solve memory)
