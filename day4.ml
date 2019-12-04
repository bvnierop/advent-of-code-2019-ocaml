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

let identity x = x

let list_min ~(by: 'a -> 'b) list =
  let rec find_min current_min current_min_value remaining =
    match remaining with
    | [] -> current_min
    | x::xs -> let current_value = by x in
      if current_value < current_min_value then
        find_min x current_value xs
      else
        find_min current_min current_min_value xs
  in
  let first_item = CCList.hd list in
  find_min first_item (by first_item) list

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Printf.printf "Execution time: %f seconds\n"
                (Unix.gettimeofday () -. t);
  res
          
(* TODO: Extract above to code reuse *)

let read_input () =
  Scanf.scanf "%d-%d" (fun a b -> (a, b))

let solve_a ?(len = 6) first last =
  let rec dfs depth last_digit current has_double =
    if depth = len then begin
      if current >= first && current <= last && has_double then begin
        (* print_endline (string_of_int current); *)
        1
        end
      else 0
    end
    else begin
      let digits = CCList.range (max 1 last_digit) 9 in
      CCList.fold_left (fun count digit ->
          let double = (has_double || (digit = last_digit)) in
          count + dfs (depth + 1) digit (current * 10 + digit) double 
          ) 0 digits
    end
  in
  let total = dfs 0 0 0 false in
  Printf.printf "%d possible passwords\n" total

let _ =
  let (first, last) = read_input () in
  time (fun () -> solve_a first last);
  
