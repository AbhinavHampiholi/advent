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

let prev_pos (r, c) = function
  | Up -> next_pos (r, c) Down 
  | Right -> next_pos (r, c) Left 
  | Left -> next_pos (r, c) Right 
  | Down -> next_pos (r, c) Up 

let in_bounds grid (r, c) =
  r >= 0 && r < Array.length grid && 
  c >= 0 && c < Array.length grid.(0)

let print_path grid visited =
  let debug_grid = Array.map Array.copy grid in
  Hashtbl.iter (fun (r, c) _ -> 
    if debug_grid.(r).(c) = '.' then debug_grid.(r).(c) <- 'O'
  ) visited;
  Array.iter (fun row ->
    Array.iter (Printf.printf "%c") row;
    Printf.printf "\n"
  ) debug_grid

let count_path grid start =
  let visited = Hashtbl.create 100 in
  let rec walk pos dir count =
    if not (in_bounds grid pos) then begin
      print_path grid visited;
      count
    end else
      let (r, c) = pos in
      if grid.(r).(c) = '#' then
        let new_dir = turn_right dir in
        let back_step = prev_pos (r, c) dir in 
        walk (next_pos back_step new_dir) new_dir count
      else begin
        let inc = if Hashtbl.mem visited (r, c) then 0 else 1 in
        Hashtbl.replace visited (r, c) true;
        walk (next_pos (r,c) dir) dir (count + inc) 
      end
  in
  walk start Up 0

let () =
  let grid = read_grid "p6.example" in
  match find_guard grid with
  | Some start -> Printf.printf "%d\n" (count_path grid start)
  | None -> Printf.printf "No guard found\n"