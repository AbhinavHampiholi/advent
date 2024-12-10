let (>>) f g x = g (f x)

let check_sequence lst =
  let rec check_adjacent increasing = function
    | [] | [_] -> true
    | a::b::rest -> 
        let diff = abs(b - a) in
        let valid = diff >= 1 && diff <= 3 && (if increasing then b > a else b < a) in
        valid && check_adjacent increasing (b::rest)
  in
  
  let check_normal lst = match lst with
    | [] | [_] -> true
    | a::b::_ as l -> 
        if b > a then check_adjacent true l
        else if b < a then check_adjacent false l
        else false
  in

  let rec remove_at i = function
    | [] -> []
    | h::t -> if i = 0 then t else h :: remove_at (i-1) t 
  in

  let rec try_all_skips i =
    i < List.length lst && (check_normal (remove_at i lst) || try_all_skips (i + 1))
  in
  
  check_normal lst || try_all_skips 0

let () = 
  let ic = open_in "p2.input" in
  let len = in_channel_length ic in
  really_input_string ic len |> 
  String.split_on_char '\n' |>
  List.map (String.split_on_char ' ' >> List.filter ((<>) "") >> List.map int_of_string) |>
  List.filter check_sequence |>
  List.length |>
  Printf.printf "Number of valid sequences: %d\n"