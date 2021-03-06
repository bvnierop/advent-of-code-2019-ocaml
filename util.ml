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

let read_next_int64 () =
  Scanf.scanf "%Ld" (fun i -> i)

let read_next_int64_opt () =
  try Some (read_next_int64 ())
  with _ -> None

let read_all_lines () =
  read_all read_line

let read_char () =
  Scanf.scanf "%c" (fun c -> c)

let read_char_opt () =
  try Some(read_char ())
  with _ -> None

let read_digit () =
  CCString.make 1 (read_char ()) |> int_of_string

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

let list_max ~(by: 'a -> 'b) list =
  let rec find_max current_max current_max_value remaining =
    match remaining with
    | [] -> current_max
    | x::xs -> let current_value = by x in
      if current_value > current_max_value then
        find_max x current_value xs
      else
        find_max current_max current_max_value xs
  in
  let first_item = CCList.hd list in
  find_max first_item (by first_item) list

let interleave_lists lists =
  let extract lists =
    let (elts, remaining) = CCList.map CCList.hd_tl lists
    |> CCList.split in
    let cleaned = CCList.filter (fun sublist -> not (CCList.is_empty sublist)) remaining in
    (elts, cleaned)
  in
  let rec extract_all acc lists =
    match lists with
    | [] -> acc
    | _ -> let (elts, remaining) = extract lists in
      extract_all (CCList.append acc elts) remaining
  in
  extract_all [] lists

let time f =
  let t = Unix.gettimeofday () in
  let res = f () in
  Printf.printf "Execution time: %f seconds"
                (Unix.gettimeofday () -. t);
  print_endline "";
  res

let is_even n = 
  n mod 2 = 0

(* https://en.wikipedia.org/wiki/Exponentiation_by_squaring *)
let pow base exponent =
  if exponent < 0 then invalid_arg "exponent can not be negative" else
  let rec aux accumulator base = function
    | 0 -> accumulator
    | 1 -> base * accumulator
    | e when is_even e -> aux accumulator (base * base) (e / 2)
    | e -> aux (base * accumulator) (base * base) ((e - 1) / 2) in
  aux 1 base exponent

let rec gcd a b =
  if b = 0 then a else gcd b (a mod b)

let lcm a b =
  match a, b with
  | 0, _ | _, 0 -> 0
  | a, b -> abs (a * b) / (gcd a b)

let div_ceil n d =
  let quotient = (n + d - 1) / d in
  let remainder = n mod d in
  if remainder = 0 then (quotient, remainder)
  else (quotient, 0 - d + remainder)

let pp_list ?(start="") ?(stop="") ?(sep=", ") pp_item fmt l =
  let rec print fmt l = match l with
    | x::((_::_) as l) ->
      pp_item fmt x;
      Format.pp_print_string fmt sep;
      Format.pp_print_cut fmt ();
      print fmt l
    | x::[] -> pp_item fmt x; Format.pp_print_cut fmt ()
    | [] -> ()
  in
  Format.pp_print_string fmt start;
  print fmt l;
  Format.pp_print_string fmt stop
