let (>>) f g x = g (f x)

let rec find_first x lst start finish =
  if start > finish then -1 else
  let mid = (start + finish) / 2 in
  if List.nth lst mid = x && (mid = 0 || List.nth lst (mid-1) < x) then mid
  else if List.nth lst mid >= x then find_first x lst start (mid-1)
  else find_first x lst (mid+1) finish

let rec find_last x lst start finish =
  if start > finish then -1 else
  let mid = (start + finish) / 2 in
  if List.nth lst mid = x && (mid = finish || List.nth lst (mid+1) > x) then mid
  else if List.nth lst mid <= x then find_last x lst (mid+1) finish
  else find_last x lst start (mid-1)

let count_occurrences x lst =
  let len = List.length lst in
  let first = find_first x lst 0 (len-1) in
  if first = -1 then 0
  else find_last x lst first (len-1) - first + 1


let () = 
  let ic = open_in "p1.input" in
  let len = in_channel_length ic in
  let nums = really_input_string ic len |> 
    String.split_on_char '\n' |>
    List.map (String.split_on_char ' ' >> List.filter ((<>) "") >> List.map int_of_string) in
      close_in ic;
      let col1, col2 = List.split (List.map (function [a;b] -> (a,b) | _ -> failwith "bad input") nums) in
      let sorted_col2 = List.sort compare col2 in  (* Sort col2 once instead of every time *)
      List.sort compare col1 |>
      List.fold_left (fun acc a -> acc + a * (count_occurrences a sorted_col2)) 0 |>
      Printf.printf "Similarity: %d\n"