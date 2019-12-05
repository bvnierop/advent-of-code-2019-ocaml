open Util

let read_input () =
  Scanf.scanf "%d-%d" (fun a b -> (a, b))

let solve_a ?(len = 6) first last =
  let rec gen depth last_digit current has_double =
    if depth = len then
      if current >= first && current <= last && has_double then 1 else 0
    else begin
      let digits = CCList.range (max 1 last_digit) 9 in
      CCList.fold_left (fun count digit ->
          let double = (has_double || (digit = last_digit)) in
          count + gen (depth + 1) digit (current * 10 + digit) double 
        ) 0 digits
    end
  in
  let total = gen 0 0 0 false in
  Printf.printf "%d possible passwords\n" total

let solve_b ?(len = 6) first last =
  let rec gen depth last_digit second_to_last_digit current double_index =
    if depth = len then
      if current >= first && current <= last && double_index >= 0 then 1 else 0
    else begin
      let digits = CCList.range (max 1 last_digit) 9 in
      CCList.fold_left (fun count digit ->
          let double = if double_index = -1 && digit = last_digit && digit != second_to_last_digit then depth - 1
            else if double_index = depth - 2 && digit = last_digit && digit = second_to_last_digit then -1
            else double_index in
          count + gen (depth + 1) digit last_digit (current * 10 + digit) double
        ) 0 digits
    end
  in
  let total = gen 0 0 0 0 (-1) in
  Printf.printf "%d possible passwords\n" total

let _ =
  let (first, last) = read_input () in
  time (fun () -> solve_a first last);
  time (fun () -> solve_b first last);

