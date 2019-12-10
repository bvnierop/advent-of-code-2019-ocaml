open Util

(* Memory things *)
let read_number_opt () =
  let number = read_next_int64_opt () in
  let _ = read_char_opt () in
  number

module IntMap = CCMap.Make(struct type t = int64 let compare = Int64.compare end)
type memory = Memory of int64 IntMap.t

let read_memory () =
  read_all_opt read_number_opt
  |> CCList.foldi (fun acc idx elt -> IntMap.add (Int64.of_int idx) elt acc) IntMap.empty
  |> (fun arr -> Memory arr)

let memory_get at (Memory memory) =
  IntMap.get_or ~default: 0L at memory

let memory_update dest value (Memory memory) =
  Memory (IntMap.add dest value memory)

(* Parameters *)
type parameter =
  | Position of int64
  | Immediate of int64
[@@deriving show]

(* Program things *)
type outputs = int64 list [@@deriving show]
type run_state =
  | Running
  | WaitingForInput
  | Terminated
type program = {
  memory: memory;
  ip: int64;
  run_state: run_state;
  input: int64 Gen.t;
  outputs: outputs;
}

let program_make ?(input = Gen.empty) memory =
  { memory = memory; ip = 0L; run_state = Running; input = input; outputs = [] }
let program_output program = memory_get 0L program.memory
let program_update dest value program =
  { program with memory = memory_update dest value program.memory }

let program_read parameter program =
  match parameter with
  | Position src -> memory_get src program.memory
  | Immediate value -> value

let program_with_input_gen gen program =
  let with_gen = { program with input = gen } in
  match program.run_state with
  | WaitingForInput -> { with_gen with run_state = Running }
  | _ -> program

let program_write parameter value program =
  match parameter with
  | Position dst -> { program with memory = memory_update dst value program.memory }
  | Immediate _ -> failwith "Cannot write with immediate mode"

let program_next_input program =
  Gen.get program.input

let program_last_output program =
  CCList.hd program.outputs

(* Inastruction things *)
type instruction =
  | Add of (parameter * parameter * parameter)
  | Multiply of (parameter * parameter * parameter)
  | Input of parameter
  | Output of parameter
  | JumpIfTrue of (parameter * parameter)
  | JumpIfFalse of (parameter * parameter)
  | LessThan of (parameter * parameter * parameter)
  | Equals of (parameter * parameter * parameter)
  | Terminate
[@@deriving show]

let instruction_size instruction =
  match instruction with
  | Add _
  | LessThan _
  | Equals _
  | Multiply _ -> 4L
  | Output _ -> 2L
  | Terminate -> 1L
  | Input _
  | JumpIfTrue _
  | JumpIfFalse _ -> 0L

let program_with_next_ip instruction program =
  { program with ip = Int64.add (program.ip) (instruction_size instruction) }

let instruction_make_updater op =
  fun a b dest program ->
  let val_a = program_read a  program in
  let val_b = program_read b  program in
  program_write dest (op val_a val_b) program

let instruction_execute_multiply = instruction_make_updater Int64.mul
let instruction_execute_add = instruction_make_updater Int64.add

let instruction_execute_input dest program =
  let input_value = program_next_input program in
  match input_value with
  | Some value -> let next_state = program_write dest value program in
    { next_state with ip = Int64.add next_state.ip 2L }
  | None -> { program with run_state = WaitingForInput }

let instruction_execute_output src program =
  let output_value = program_read src program in
  { program with outputs = output_value :: program.outputs }

let instruction_execute_jump_if cmp src dst program =
  let value = program_read src program in
  if cmp value = true then
    { program with ip = program_read dst program }
  else
    { program with ip = Int64.add program.ip 3L }

let instruction_execute_cmp cmp a b dst program =
  let val_a = program_read a program in
  let val_b = program_read b program in
  let result = cmp val_a val_b in
  program_write dst result program

let instr_less_than a b = if a < b then 1L else 0L
let instr_eql a b = if a = b then 1L else 0L

let instruction_execute program instruction =
  let after_instruction = match instruction with
  | Add (a, b, dest) -> instruction_execute_add a b dest program
  | Multiply (a, b, dest) -> instruction_execute_multiply a b dest program
  | Input dest -> instruction_execute_input dest program
  | Output src -> instruction_execute_output src program
  | JumpIfTrue (src, dst) -> instruction_execute_jump_if (fun v -> v != 0L) src dst program
  | JumpIfFalse (src, dst) -> instruction_execute_jump_if (fun v -> v = 0L) src dst program
  | LessThan (a, b, dst) -> instruction_execute_cmp instr_less_than a b dst program
  | Equals (a, b, dst) -> instruction_execute_cmp instr_eql a b dst program
  | Terminate -> { program with run_state = Terminated } in
  program_with_next_ip instruction after_instruction

let parse_parameter opcode n program =
  let mode = (opcode / (100 * (pow 10 n))) mod 10 in
  let value = memory_get (Int64.add program.ip (Int64.add (Int64.of_int n) 1L)) program.memory in
  match mode with
  | 0 -> Position value
  | 1 -> Immediate value
  | i -> failwith (Printf.sprintf "Invalid memory mode: %d" i)

let one_parameter opcode program =
  parse_parameter opcode 0 program
    
let two_parameters opcode program =
  ((parse_parameter opcode 0 program),
   (parse_parameter opcode 1 program))

let three_parameters opcode program =
  ((parse_parameter opcode 0 program),
   (parse_parameter opcode 1 program),
   (parse_parameter opcode 2 program))

let next_instruction_of_program (program: program) =
  let opcode = Int64.to_int (memory_get program.ip program.memory) in
  match (opcode mod 100) with 
  | 1 -> Add (three_parameters opcode program)
  | 2 -> Multiply (three_parameters opcode program)
  | 3 -> Input (one_parameter opcode program)
  | 4 -> Output (one_parameter opcode program)
  | 5 -> JumpIfTrue (two_parameters opcode program)
  | 6 -> JumpIfFalse (two_parameters opcode program)
  | 7 -> LessThan (three_parameters opcode program)
  | 8 -> Equals (three_parameters opcode program)
  | 99 -> Terminate
  | i -> (Printf.printf "Unknown opcode: %d\n" i); Terminate

let rec program_execute ?(verbose = false) ?(print_outputs = false)  program =
  if program.run_state != Running then begin
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

