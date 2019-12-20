open Util

let list_explode n list =
  Gen.fold (fun acc _ -> list :: acc) [] (Gen.int_range 0 n)
  |> interleave_lists

let phase_matrix digits =
  let digit_count = CCList.length digits in
  let gen = Gen.int_range 0 (digit_count - 1) in
  Gen.fold (fun acc n ->
      let len = (n + 1) * 4 in
      let repeats = (digit_count / len + 1) in
      let exploded = list_explode n [0; 1; 0; -1] in
      let repeated = CCList.repeat repeats exploded in
      let shifted = CCList.tl repeated in
      let truncated = CCList.take digit_count shifted in
      truncated :: acc) [] gen
  |> CCList.rev

let phase matrix digits =
  CCList.map (fun pattern ->
      (abs
         (CCList.fold_left2 (fun sum p d -> sum + d * p) 0 pattern digits)) mod 10)
    matrix

let phases n matrix digits =
  if n = 0 then digits
  else Gen.fold (fun state _ -> phase matrix state) digits (Gen.int_range 0 (n - 1))

let solve digits =
  let matrix = phase_matrix digits in
  let after_phase = phases 100 matrix digits in
  let first_eight = CCList.take 8 after_phase in
  CCFormat.printf "%a@." (pp_list ~sep: "" CCInt.pp) first_eight


let offset digits =
  let offset_digits = CCList.take 7 digits in
   CCList.fold_left (fun o digit -> o * 10 + digit) 0 offset_digits

(*
Observation:
     The offset is larger than half the length of the signal
     The pattern ensures we can just sum from the back
     
These observations mean we can simply ignore the first half (leave it 0)
and only calculate the second half. In fact, we only have to calculate
from the back up to offset.
*)
let phase_backwards_sum digits =
  (*
  8 7 6 5 ->
        [] 0 8 -> [8] 8
        [8] 8 7 -> [8, 5] 15
        [8, 5] ...
  *)
  let (digits, _) = CCList.fold_left (fun (new_list, partial_sum) digit ->
      let new_sum = partial_sum + digit in
      ((new_sum mod 10) :: new_list, new_sum)) ([], 0) (CCList.rev digits) in
  digits

let solve_b digits =
  let offset = offset digits in
  let digits = CCList.repeat 10000 digits in
  let sublist = CCList.drop offset digits in
  Gen.fold (fun state _ -> phase_backwards_sum state) sublist (Gen.int_range 0 99)
  |> (fun list -> CCFormat.printf "%a@." (pp_list ~sep: "" CCInt.pp) (CCList.take 8 list))
  
let read_digits () =
  read_all read_digit

let _ =
  let digits = read_digits () in
  time (fun () -> solve digits);
  time (fun () -> solve_b digits)

