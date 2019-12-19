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

let read_digits () =
  read_all read_digit

let _ =
  let digits = read_digits () in
  time (fun () -> solve digits)

