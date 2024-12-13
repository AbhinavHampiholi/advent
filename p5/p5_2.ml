(* OCaml program to validate and reorder sequences *)

let read_input filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  let parts = String.split_on_char '\n' content |> List.filter ((<>) "") in
  
  let rec split_parts rules = function
    | [] -> (List.rev rules, [])
    | line::rest ->
        if String.contains line ',' then
          (List.rev rules, line::rest)
        else
          split_parts (line::rules) rest
  in
  split_parts [] parts

let parse_rule line =
  match String.split_on_char '|' line with
  | [x; y] -> (int_of_string x, int_of_string y)
  | _ -> failwith ("Invalid rule format: " ^ line)

let parse_list line =
  String.split_on_char ',' line |> List.map int_of_string

(* Check if a list satisfies a single rule (x must not occur after y) *)
let check_rule lst (x, y) =
  let rec check_no_x_after_y found_y = function
    | [] -> true
    | n::rest ->
        if n = y then
          check_no_x_after_y true rest
        else if n = x && found_y then
          false
        else 
          check_no_x_after_y found_y rest
  in
  check_no_x_after_y false lst

(* Check if a list satisfies all rules *)
let check_all_rules rules lst =
  List.for_all (check_rule lst) rules

(* Helper function to swap two elements in an array *)
let swap arr i j =
  let temp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- temp

(* Find indices of x and y in array, return None if either not found *)
let find_indices arr x y =
  let x_idx = ref None in
  let y_idx = ref None in
  Array.iteri (fun i n ->
    if n = x then x_idx := Some i
    else if n = y then y_idx := Some i
  ) arr;
  match !x_idx, !y_idx with
  | Some xi, Some yi -> Some (xi, yi)
  | _ -> None

(* Reorder a list to satisfy all rules *)
let reorder_list rules lst =
  let arr = Array.of_list lst in
  let len = Array.length arr in
  let rec fix_violations changed =
    let new_changed = ref false in
    List.iter (fun (x, y) ->
      match find_indices arr x y with
      | Some (xi, yi) when xi > yi -> (* x appears after y - violation *)
          swap arr xi yi;
          new_changed := true
      | _ -> ()
    ) rules;
    if !new_changed then
      fix_violations true
    else
      changed
  in
  let changed = fix_violations false in
  (Array.to_list arr, changed)

(* Get middle element of a list *)
let get_middle lst =
  let len = List.length lst in
  List.nth lst (len / 2)

(* Main program *)
let () =
  let rules_part, lists_part = read_input "p5.input" in
  let rules = List.map parse_rule rules_part in
  let lists = List.map parse_list lists_part in
  
  let sum = List.fold_left (fun acc lst ->
    if not (check_all_rules rules lst) then
      let reordered, changed = reorder_list rules lst in
      if changed then
        acc + get_middle reordered
      else
        acc
    else
      acc
  ) 0 lists in
  
  Printf.printf "%d\n" sum