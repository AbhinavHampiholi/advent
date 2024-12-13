(* OCaml program to validate sequences against ordering rules *)

(* Read input file and split into rules and lists *)
let read_input filename =
  let ic = open_in filename in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  let parts = String.split_on_char '\n' content |> List.filter ((<>) "") in
  
  (* Find the first line that contains a comma - that's where lists start *)
  let rec split_parts rules = function
    | [] -> (List.rev rules, [])
    | line::rest ->
        if String.contains line ',' then
          (List.rev rules, line::rest)
        else
          split_parts (line::rules) rest
  in
  split_parts [] parts

(* Parse a rule of the form "47|53" *)
let parse_rule line =
  match String.split_on_char '|' line with
  | [x; y] -> (int_of_string x, int_of_string y)
  | _ -> failwith ("Invalid rule format: " ^ line)

(* Parse a list of the form "75,47,61,53,29" *)
let parse_list line =
  String.split_on_char ',' line |> List.map int_of_string

(* Check if a list satisfies a single rule (x must not occur after y) *)
let check_rule lst (x, y) =
  let rec check_no_x_after_y found_y = function
    | [] -> true  (* reached end without finding x after y *)
    | n::rest ->
        if n = y then
          check_no_x_after_y true rest
        else if n = x && found_y then
          false  (* found x after y - rule violated *)
        else 
          check_no_x_after_y found_y rest
  in
  check_no_x_after_y false lst

(* Check if a list satisfies all rules *)
let check_all_rules rules lst =
  List.for_all (check_rule lst) rules

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
    if check_all_rules rules lst then
      acc + get_middle lst
    else
      acc
  ) 0 lists in
  
  Printf.printf "Sum of mids: %d\n" sum