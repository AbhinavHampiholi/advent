let read_input filename =
  let ic = open_in filename in
  let content = really_input_string ic (in_channel_length ic) in
  close_in ic;
  String.trim content

let make_position_map input =
  let digits = String.fold_right (fun c acc -> 
    int_of_string (String.make 1 c) :: acc) input [] in
  
  let rec build_map pos file_id acc = function
    | [] -> acc
    | file_size :: space :: rest ->
        let file_positions = List.init file_size (fun i -> (pos + i, Some file_id)) in
        let empty_positions = List.init space (fun i -> 
          (pos + file_size + i, None)) in
        build_map (pos + file_size + space) (file_id + 1) 
          (file_positions @ empty_positions @ acc) rest
    | [file_size] ->
        let file_positions = List.init file_size (fun i -> (pos + i, Some file_id)) in
        file_positions @ acc
  in
  build_map 0 0 [] digits
  |> List.sort (fun (a,_) (b,_) -> compare a b)

let defragment map =
  let arr = Array.of_list map in
  let len = Array.length arr in
  
  (* For each empty space *)
  for i = 0 to len - 2 do
    if snd arr.(i) = None then
      (* Find rightmost non-empty block *)
      let j = ref (len - 1) in
      while !j > i && snd arr.(!j) = None do
        decr j
      done;
      if !j > i then begin
        arr.(i) <- (!j, snd arr.(!j));
        arr.(!j) <- (!j, None)
      end
  done;
  Array.to_list arr

let () =
  let input = read_input "p9.input" in
  let position_map = make_position_map input in
  let defragged_map = defragment position_map in
  
  (* List.iter (fun (pos, id_opt) ->
    Printf.printf "%d : %s\n" pos
      (match id_opt with 
       | Some id -> string_of_int id
       | None -> ".")
  ) defragged_map; *)

  let sum = List.fold_left (fun (pos, acc) (_, id_opt) ->
    match id_opt with
    | Some id -> (pos + 1, acc + (pos * id))
    | None -> (pos + 1, acc)
  ) (0, 0) defragged_map in
  
  Printf.printf "%d\n" (snd sum)