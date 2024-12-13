
type direction = Up | Right | Down | Left

let read_grid filename =
  let ic = open_in filename in
  let lines = ref [] in
  try
    while true do
      lines := input_line ic :: !lines
    done;
    [||]
  with End_of_file ->
    close_in ic;
    List.rev !lines |> List.map String.to_seq |> List.map Array.of_seq |> Array.of_list

let find_guard grid =
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let rec find_pos r c =
    if r >= rows then None
    else if c >= cols then find_pos (r + 1) 0
    else if grid.(r).(c) = '^' then Some (r, c)
    else find_pos r (c + 1)
  in
  find_pos 0 0

let turn_right = function
  | Up -> Right
  | Right -> Down
  | Down -> Left
  | Left -> Up

let next_pos (r, c) = function
  | Up -> (r - 1, c)
  | Right -> (r, c + 1)
  | Down -> (r + 1, c)
  | Left -> (r, c - 1)

let in_bounds grid (r, c) =
  r >= 0 && r < Array.length grid && 
  c >= 0 && c < Array.length grid.(0)

let check_path_exit grid start_pos start_dir =
  let visited = Hashtbl.create 100 in
  let rec simulate pos dir =
    if not (in_bounds grid (next_pos pos dir)) then
      true
    else
      let (rn, cn) = next_pos pos dir in
      let state = (pos, dir) in
      if Hashtbl.mem visited state then
        false
      else 
        begin
        Hashtbl.add visited state true;
        if grid.(rn).(cn) = '#' then
          let new_dir = turn_right dir in
          simulate pos new_dir
        else
          simulate (next_pos pos dir) dir
        end
  in
  simulate start_pos start_dir
  
let test_obstacle_position grid start pos =
  (* Create a copy of the grid with new obstacle *)
  let test_grid = Array.map Array.copy grid in
  let (r, c) = pos in
  test_grid.(r).(c) <- '#';
  not (check_path_exit test_grid start Up)
  
let count_path grid start =
  let visited = Hashtbl.create 100 in
  let visited_positions = ref [] in
  let rec walk pos dir count =
    begin 
      let inc = if Hashtbl.mem visited pos then 0 else 1 in
      if inc = 1 then visited_positions := pos :: !visited_positions;
      Hashtbl.replace visited pos true;
      let (r, c) = next_pos pos dir in
      if not (in_bounds grid (r,c)) then
        (count+inc, !visited_positions)
      else 
      if grid.(r).(c) = '#' then
        begin
          let new_dir = turn_right dir in
          walk pos new_dir (count+inc)
        end
      else 
        walk (next_pos pos dir) dir (count + inc)
    end
  in
  walk start Up 0 


let remove_duplicates lst =
  let tbl = Hashtbl.create (List.length lst) in
  List.iter (fun x -> Hashtbl.replace tbl x true) lst;
  Hashtbl.fold (fun k _ acc -> k :: acc) tbl []

let count_loop_positions grid start visited_positions =
  let visited_list = remove_duplicates visited_positions in 
  List.filter (fun pos -> pos <> start && test_obstacle_position grid start pos) visited_list
  |> List.length

let () =
  let grid = read_grid "p6.input" in
  match find_guard grid with
  | Some start -> 
    (begin
      let (c, visited) = (count_path grid start) in
      Printf.printf "(%d)\n" c;
      let loop_count = count_loop_positions grid start visited in
      Printf.printf "%d positions create loops\n" loop_count
    end)
  | None -> Printf.printf "No guard found\n"