open Util

module IntMap = CCMap.Make(struct type t = int let compare = Stdlib.compare end)

let counts layer =
  CCList.fold_left (fun counts digit ->
      let cur_count = IntMap.get_or ~default: 0 digit counts in
      IntMap.add digit (cur_count + 1) counts)
    IntMap.empty layer

let solve digits =
  CCList.sublists_of_len (25 * 6) digits
  |> CCList.map counts
  |> list_min ~by: (fun counts -> IntMap.get_or ~default: 0 0 counts)
  |> (fun counted_layer ->
      (IntMap.get_or ~default: 0 1 counted_layer) *
      (IntMap.get_or ~default: 0 2 counted_layer))
  |> string_of_int
  |> print_endline

let draw digits =
  CCList.sublists_of_len (25 * 6) digits
  |> CCList.rev
  |> CCList.fold_left (fun final layer ->
      CCList.map2 (fun f l -> if l = 2 then f else l) final layer)
    (CCList.init (25 * 6) (fun _ -> 0))
  |> CCList.sublists_of_len 25
  |> CCList.map (fun sublist ->
      CCList.map (fun d -> if d = 1 then "O" else " ") sublist |> CCString.concat "")
  |> CCString.concat "\n"
  |> print_endline

let _ =
  let digits = read_all read_digit in
  time (fun () -> solve digits);
  time (fun () -> draw digits)
