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
  
  (* Count consecutive empty spaces starting at position i *)
  let count_empty_at i =
    let count = ref 0 in
    let pos = ref i in
    while !pos < len && snd arr.(!pos) = None do
      incr count;
      incr pos
    done;
    !count
  in

  (* Count size of file with ID *)
  let count_file_size id start =
    let count = ref 0 in
    let pos = ref start in
    while !pos < len && snd arr.(!pos) = Some id do
      incr count;
      incr pos
    done;
    !count
  in

  (* Process files in decreasing ID order *)
  let max_id = Array.fold_left (fun acc (_, id_opt) ->
    match id_opt with
    | Some id -> max id acc
    | None -> acc
  ) 0 arr in

  for id = max_id downto 0 do
    (* Find where this file starts *)
    let file_start = ref 0 in
    while !file_start < len && snd arr.(!file_start) <> Some id do
      incr file_start
    done;
    if !file_start < len then begin
      let file_size = count_file_size id !file_start in
      (* Look for empty space before this file *)
      let pos = ref 0 in
      while !pos < !file_start do
        let empty_size = count_empty_at !pos in
        if empty_size >= file_size then begin
          (* Move the file here *)
          for i = 0 to file_size - 1 do
            arr.(!pos + i) <- (!pos + i, Some id);
            arr.(!file_start + i) <- (!file_start + i, None)
          done;
          pos := len  (* Exit loop *)
        end else
          incr pos
      done
    end
  done;
  Array.to_list arr

let () =
  let input = read_input "p9.input" in
  let position_map = make_position_map input in
  let defragged_map = defragment position_map in
  
  let sum = List.fold_left (fun (pos, acc) (_, id_opt) ->
    match id_opt with
    | Some id -> (pos + 1, acc + (pos * id))
    | None -> (pos + 1, acc)
  ) (0, 0) defragged_map in
  
  Printf.printf "%d\n" (snd sum)