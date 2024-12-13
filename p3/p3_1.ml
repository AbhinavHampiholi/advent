open Str

let (>>) f g x = g (f x)

(* Helper functions *)
let validate_numbers x y =
  try
    let nx = int_of_string x in
    let ny = int_of_string y in
    nx >= 0 && nx <= 999 && ny >= 0 && ny <= 999
  with Failure _ -> false

(* Function to extract and validate multiplication expressions *)
let find_multiplications input =
  let regexp = regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" in
  let rec find_all pos acc =
    try
      let next_pos = search_forward regexp input pos in
      let x = matched_group 1 input in
      let y = matched_group 2 input in
      let matched = matched_string input in
      if validate_numbers x y then
        find_all (next_pos + 1) ((matched, int_of_string x, int_of_string y) :: acc)
      else
        find_all (next_pos + 1) acc
    with Not_found -> 
      List.rev acc
  in
  find_all 0 []

(* Main program *)
let () =
  let ic = open_in "p3.example" in
  let len = in_channel_length ic in
  really_input_string ic len |>
  find_multiplications |>
  (fun results ->
    let sum = List.fold_left (fun acc (_, x, y) -> acc + (x * y)) 0 results in
    Printf.printf "Sum of all products: %d\n" sum
  )