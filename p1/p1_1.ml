let (>>) f g x = g (f x)

let () = 
  let ic = open_in "p1.input" in
  let len = in_channel_length ic in
  let nums = really_input_string ic len |> 
    String.split_on_char '\n' |>
    List.map (String.split_on_char ' ' >> List.filter ((<>) "") >> List.map int_of_string) in
      close_in ic;
      let col1, col2 = List.split (List.map (function [a;b] -> (a,b) | _ -> failwith "bad input") nums) in
      List.combine (List.sort compare col1) (List.sort compare col2) |>
      List.fold_left (fun acc (a,b) -> acc + abs(a-b)) 0 |>
      Printf.printf "Sum: %d\n"