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
  let mul_regexp = regexp "mul(\\([0-9]+\\),\\([0-9]+\\))" in
  let do_regexp = regexp "do()" in
  let dont_regexp = regexp "don't()" in
  
  let rec find_next_control pos =
    try
      let do_pos = search_forward do_regexp input pos in
      try
        let dont_pos = search_forward dont_regexp input pos in
        if do_pos < dont_pos then
          Some (true, do_pos)
        else
          Some (false, dont_pos)
      with Not_found -> Some (true, do_pos)
    with Not_found ->
      try
        let dont_pos = search_forward dont_regexp input pos in
        Some (false, dont_pos)
      with Not_found -> None
  in
  
  let rec find_all pos enabled acc =
    match find_next_control pos with
    | Some (new_state, control_pos) ->
        (* Process multiplications up to the control instruction *)
        let mul_acc = if enabled then
          let rec process_muls cur_pos acc =
            try
              let next_pos = search_forward mul_regexp input cur_pos in
              if next_pos >= control_pos then acc
              else
                let x = matched_group 1 input in
                let y = matched_group 2 input in
                let matched = matched_string input in
                if validate_numbers x y then
                  process_muls (next_pos + 1) ((matched, int_of_string x, int_of_string y) :: acc)
                else
                  process_muls (next_pos + 1) acc
            with Not_found -> acc
          in
          process_muls pos acc
        else acc
        in
        (* Continue searching after the control instruction *)
        find_all (control_pos + 1) new_state mul_acc
    | None ->
        (* Process remaining multiplications if enabled *)
        if enabled then
          let rec process_muls pos acc =
            try
              let next_pos = search_forward mul_regexp input pos in
              let x = matched_group 1 input in
              let y = matched_group 2 input in
              let matched = matched_string input in
              if validate_numbers x y then
                process_muls (next_pos + 1) ((matched, int_of_string x, int_of_string y) :: acc)
              else
                process_muls (next_pos + 1) acc
            with Not_found -> acc
          in
          List.rev (process_muls pos acc)
        else
          List.rev acc
  in
  find_all 0 true []

(* Print results *)
let print_results = List.iter (fun (expr, x, y) ->
  Printf.printf "Found: %s (numbers: %d, %d)\n" expr x y;
  Printf.printf "Result: %d\n\n" (x * y)
)

(* Main program *)
let () =
  let ic = open_in "p3.input" in
  let len = in_channel_length ic in
  really_input_string ic len |>
  find_multiplications |>
  (fun results ->
    print_results results;
    Printf.printf "Total expressions found: %d\n" (List.length results);
    let sum = List.fold_left (fun acc (_, x, y) -> acc + (x * y)) 0 results in
    Printf.printf "Sum of all products: %d\n" sum
  )