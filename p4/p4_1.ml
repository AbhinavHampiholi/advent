(* Read grid from input file *)
let read_grid filename =
  let ic = open_in filename in
  let rec read_lines acc =
    try
      let line = input_line ic in
      read_lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  let lines = read_lines [] in
  Array.of_list (List.map String.to_seq lines |> List.map Array.of_seq)

(* Directions to check: right, down-right, down, down-left, left, up-left, up, up-right *)
let directions = [
  (0, 1);   (* right *)
  (1, 1);   (* down-right *)
  (1, 0);   (* down *)
  (1, -1);  (* down-left *)
  (0, -1);  (* left *)
  (-1, -1); (* up-left *)
  (-1, 0);  (* up *)
  (-1, 1)   (* up-right *)
]

(* Check if a position is within grid bounds *)
let in_bounds grid row col =
  row >= 0 && row < Array.length grid &&
  col >= 0 && col < Array.length grid.(0)

(* Check if XMAS appears starting from a position in a given direction *)
let check_xmas grid row col (dr, dc) =
  let check_pos r c expected =
    in_bounds grid r c && grid.(r).(c) = expected
  in
  check_pos row col 'X' &&
  check_pos (row + dr) (col + dc) 'M' &&
  check_pos (row + 2*dr) (col + 2*dc) 'A' &&
  check_pos (row + 3*dr) (col + 3*dc) 'S'

(* Count all occurrences of XMAS in the grid *)
let count_xmas grid =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let count = ref 0 in
  
  (* For each position in the grid *)
  for row = 0 to rows - 1 do
    for col = 0 to cols - 1 do
      (* If we find an X, check all 8 directions *)
      if grid.(row).(col) = 'X' then
        List.iter (fun dir ->
          if check_xmas grid row col dir then
            incr count
        ) directions
    done
  done;
  !count

(* Main program *)
let () =
  let ic = open_in "p4.input" in
  let len = in_channel_length ic in
  let content = really_input_string ic len in
  let grid = read_grid "p4.input" in
  let result = count_xmas grid in
  Printf.printf "Found %d occurrences of XMAS\n" result