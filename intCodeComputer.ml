open Util

(* Memory things *)
type memory = Memory of int CCPersistentArray.t
[@@deriving show]

let read_memory () =
  read_all_opt read_number_opt
  |> CCPersistentArray.of_list
  |> (fun arr -> Memory arr)

let memory_get (Memory memory) at =
  CCPersistentArray.get memory at
let memory_update dest value (Memory memory) =
  Memory (CCPersistentArray.set memory dest value)

(* Parameters *)
type parameter =
  | Position of int
[@@deriving show]

(* Program things *)
type outputs = int list [@@deriving show]
type inputs = int list [@@deriving show]
type program = {
  memory: memory;
  ip: int;
  terminated: bool;
  inputs: inputs;
  outputs: outputs;
}

[@@deriving show]
let program_make ?(inputs = []) memory =
  { memory = memory; ip = 0; terminated = false; inputs = inputs; outputs = [] }
let program_output program = memory_get program.memory 0
let program_update dest value program =
  { program with memory = memory_update dest value program.memory }

let program_read parameter program =
  match parameter with
  | Position src -> memory_get program.memory src

let program_write parameter value program =
  match parameter with
  | Position dst -> { program with memory = memory_update dst value program.memory }

let program_next_input program =
  let (first, rest) = CCList.hd_tl program.inputs in
  (first, { program with inputs = rest })

(* Inastruction things *)
type instruction =
  | Add of (parameter * parameter * parameter)
  | Multiply of (parameter * parameter * parameter)
  | Input of parameter
  | Output of parameter
  | Terminate
[@@deriving show]

let instruction_size instruction =
  match instruction with
  | Add _
  | Multiply _ -> 4
  | Input _
  | Output _ -> 2
  | Terminate -> 1

let program_with_next_ip instruction program =
  { program with ip = program.ip + instruction_size instruction }

let instruction_make_updater op =
  fun a b dest program ->
  let val_a = program_read a  program in
  let val_b = program_read b  program in
  program_write dest (op val_a val_b) program

let instruction_execute_multiply = instruction_make_updater ( * )
let instruction_execute_add = instruction_make_updater ( + )

let instruction_execute_input dest program =
  let (input_value, next_state) = program_next_input program in
  program_write dest input_value next_state

let instruction_execute_output src program =
  let output_value = program_read src program in
  { program with outputs = output_value :: program.outputs }
  
let instruction_execute program instruction =
  let after_instruction = match instruction with
  | Add (a, b, dest) -> instruction_execute_add a b dest program
  | Multiply (a, b, dest) -> instruction_execute_multiply a b dest program
  | Input dest -> instruction_execute_input dest program
  | Output src -> instruction_execute_output src program
  | Terminate -> { program with terminated = true } in
  program_with_next_ip instruction after_instruction

let next_instruction_of_program (program: program) =
  let oper = memory_get program.memory program.ip in
  match oper with
  | 1 -> Add((Position (memory_get program.memory (program.ip + 1))),
             (Position (memory_get program.memory (program.ip + 2))),
             (Position (memory_get program.memory (program.ip + 3))))
  | 2 -> Multiply((Position (memory_get program.memory (program.ip + 1))),
                  (Position (memory_get program.memory (program.ip + 2))),
                  (Position (memory_get program.memory (program.ip + 3))))
  | 3 -> Input (Position (memory_get program.memory (program.ip + 1)))
  | 4 -> Output (Position (memory_get program.memory (program.ip + 1)))
  | 99 -> Terminate
  | i -> (Printf.printf "Unknown opcode: %d\n" i); Terminate

let rec program_execute ?(verbose = false) ?(print_outputs = false)  program =
  if verbose then print_endline (show_program program);
  if program.terminated then begin
    if print_outputs then
      print_endline (Printf.sprintf "Program outputs: %s" (show_outputs program.outputs));
    program
  end
  else
    let instr = next_instruction_of_program program in
    if verbose then print_endline (show_instruction instr);
    program_execute ~verbose: verbose
      ~print_outputs: print_outputs
      (instruction_execute program instr)

