let read_line_opt () =
  try Some (read_line ())
  with End_of_file -> None

let read_all reader =
  let rec read acc =
    try let value = reader () in read (value :: acc)
    with _ -> List.rev acc in
  read []

let read_all_opt reader_opt =
  let rec read acc =
    match reader_opt () with
    | Some value -> read (value :: acc)
    | None -> List.rev acc in
  read []

let read_next_int () =
  Scanf.scanf "%d" (fun i -> i)

let read_next_int_opt () =
  try Some(read_next_int ())
  with _ -> None

let read_all_lines () =
  read_all read_line

let read_char () =
  Scanf.scanf "%c" (fun c -> c)

let read_char_opt () =
  try Some(read_char ())
  with _ -> None

(* TODO: Extract above to code reuse *)

(* 
Read all memory, store as persisted array
Walk through the memory, performing each step as we go
*)

(* Reads a number and swallow the trailing comma *)
let read_number_opt () =
  let number = read_next_int_opt () in
  let _ = read_char_opt () in
  number

(* Read all numbers from input *)
let read_all_numbers () = read_all_opt read_number_opt

(* Memory things *)
type memory = Memory of int CCPersistentArray.t
[@@deriving show]

let read_memory () =
  read_all_numbers ()
  |> CCPersistentArray.of_list
  |> (fun arr -> Memory arr)

let memory_get (Memory memory) at =
  CCPersistentArray.get memory at
let memory_update dest value (Memory memory) =
  Memory (CCPersistentArray.set memory dest value)

(* Program things *)
type program = {
  memory: memory;
  ip: int;
  terminated: bool;
}
[@@deriving show]
let program_make memory = { memory = memory; ip = 0; terminated = false }
let program_memory program = match program.memory with Memory m -> m
let program_update dest value program =
  { program with memory = memory_update dest value program.memory }

type instruction =
  | Add of (int * int * int)
  | Multiply of (int * int * int)
  | Terminate
[@@deriving show]

let instruction_make_updater op =
  fun a b dest program ->
  let val_a = memory_get program.memory a in
  let val_b = memory_get program.memory b in
  let program_after_instruction = program_update dest (op val_a val_b) program in
  { program_after_instruction with ip = program.ip + 4 }

let instruction_execute_multiply = instruction_make_updater ( * )
let instruction_execute_add = instruction_make_updater ( + )
  
let instruction_execute program instruction =
  match instruction with
  | Add (a, b, dest) -> instruction_execute_add a b dest program
  | Multiply (a, b, dest) -> instruction_execute_multiply a b dest program
  | Terminate -> { program with terminated = true }

let next_instruction_of_program (program: program) =
  let oper = memory_get program.memory program.ip in
  match oper with
  | 1 -> Add((memory_get program.memory (program.ip + 1)),
             (memory_get program.memory (program.ip + 2)),
             (memory_get program.memory (program.ip + 3)))
  | 2 -> Multiply((memory_get program.memory (program.ip + 1)),
                  (memory_get program.memory (program.ip + 2)),
                  (memory_get program.memory (program.ip + 3)))
  | 99 -> Terminate
  | _ -> Terminate

let rec program_execute ?(verbose: bool = false) program =
  if verbose then print_endline (show_program program);
  if program.terminated then program
  else
    let instr = next_instruction_of_program program in
    if verbose then print_endline (show_instruction instr);
    program_execute ~verbose: verbose (instruction_execute program instr)

let solve_a (memory) =
  let faulty_memory = memory_update 2 2 (memory_update 1 12 memory) in
  let final_state = program_execute ~verbose: true (program_make faulty_memory) in
  print_endline (Printf.sprintf "Final value of pos 0: %d" (memory_get final_state.memory 0))
  
let _ =
  let memory = read_memory () in
  solve_a memory
