let read_line_safe () = try Some (read_line ())
  with End_of_file -> None

let read_all_lines () =
  let rec read acc =
    match read_line_safe () with
    | Some line -> read (line :: acc)
    | None -> List.rev acc in
  read []

let read_modules () =
  read_all_lines () |> List.map int_of_string

let fuel_for_weight w =
  w / 3 - 2

let fuel_for_module m =
  fuel_for_weight m

let adjusted_fuel_for_module m =
  let rec iter remaining_weight total_fuel =
    let fuel_to_add = fuel_for_weight remaining_weight in
    if fuel_to_add <= 0 then total_fuel
    else iter fuel_to_add (total_fuel + fuel_to_add) in
  iter m 0

let solve modules fuel_formula =
  modules
  |> List.map fuel_formula
  |> List.fold_left (+) 0
  |> string_of_int |> print_endline
  
let _ =
  let modules = read_modules () in
  solve modules fuel_for_module;
  solve modules adjusted_fuel_for_module
