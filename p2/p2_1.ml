let (>>) f g x = g (f x)

let check_sequence lst =
  let rec check_adjacent increasing = function
    | [] | [_] -> true  (* Empty or single element list is valid *)
    | a::b::rest -> 
        let diff = abs(b - a) in
        let valid_order = if increasing then b > a else b < a in
        diff >= 1 && diff <= 3 && valid_order && check_adjacent increasing (b::rest)
  in
  match lst with
  | [] | [_] -> true  (* Empty or single element list is valid *)
  | a::b::_ as l -> 
      if b > a then check_adjacent true l    (* Check if increasing *)
      else if b < a then check_adjacent false l  (* Check if decreasing *)
      else false  (* Equal adjacent numbers - invalid *)

let () = 
  let ic = open_in "p2.input" in
  let len = in_channel_length ic in
  let nums = really_input_string ic len |> 
    String.split_on_char '\n' |>
    List.map (String.split_on_char ' ' >> List.filter ((<>) "") >> List.map int_of_string) in
      close_in ic;
      nums |> 
      List.filter check_sequence |>  (* Keep only the valid sequences *)
      List.length |>                 (* Count them *)
      Printf.printf "Number of valid sequences: %d\n"