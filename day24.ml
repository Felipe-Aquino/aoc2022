open Utils

let up = 1
let down = 2
let left = 4
let right = 8
let wall = 16

type grid_t = int array array

type pos_t =
  { row: int
  ; col: int
  }

type state_t =
  { grid: grid_t
  ; next_grid: grid_t
  ; nrows: int
  ; ncols: int
  ; current_positions: pos_t list
  ; end_pos: pos_t
  ; minutes: int
  ; is_done: bool
  }

let list_dedup cmp ls0 =
  let rec loop ls result =
    match ls with
    | [] -> result
    | hd :: tl ->
      if List.exists (cmp hd) tl then
        loop tl result
      else
        loop tl (hd :: result)
  in
    loop ls0 []

let pos_equal (p1: pos_t) (p2: pos_t): bool = p1.row == p2.row && p1.col == p2.col

let advance_a_minute (s: state_t): state_t =
  for i = 1 to s.nrows - 2 do
    for j = 1 to s.ncols - 2 do
      let cell = s.grid.(i).(j) in
      if Int.logand cell up != 0 then (
        let k = 1 + ((i + s.nrows - 4) mod (s.nrows - 2)) in
          s.next_grid.(k).(j) <- Int.logor s.next_grid.(k).(j) up
      );

      if Int.logand cell down != 0 then (
        let k = 1 + (i mod (s.nrows - 2)) in
          s.next_grid.(k).(j) <- Int.logor s.next_grid.(k).(j) down
      );

      if Int.logand cell left != 0 then (
        let k = 1 + ((j + s.ncols - 4) mod (s.ncols - 2)) in
          s.next_grid.(i).(k) <- Int.logor s.next_grid.(i).(k) left
      );

      if Int.logand cell right != 0 then (
        let k = 1 + (j mod (s.ncols - 2)) in
          s.next_grid.(i).(k) <- Int.logor s.next_grid.(i).(k) right
      )
    done
  done;

  for i = 1 to s.nrows - 2 do
    for j = 1 to s.ncols - 2 do
      s.grid.(i).(j) <- s.next_grid.(i).(j);
      s.next_grid.(i).(j) <- 0
    done
  done;

  let rec get_new_positions current_positions result =
    match current_positions with
    | [] ->
      list_dedup
        (fun a b -> a.row == b.row && a.col == b.col)
        result 
    | pos :: remaining ->
      let new_positions = ref [] in
      let row0 = (pos.row + s.nrows - 1) mod s.nrows in
      let row1 = (pos.row + s.nrows + 1) mod s.nrows in
      if s.grid.(row0).(pos.col) == 0 then (
        new_positions := { row = pos.row - 1; col = pos.col } :: !new_positions
      );
      if s.grid.(row1).(pos.col) == 0 then (
        new_positions := { row = pos.row + 1; col = pos.col } :: !new_positions
      );
      if s.grid.(pos.row).(pos.col - 1) == 0 then (
        new_positions := { row = pos.row; col = pos.col - 1 } :: !new_positions
      );
      if s.grid.(pos.row).(pos.col + 1) == 0 then (
        new_positions := { row = pos.row; col = pos.col + 1 } :: !new_positions
      );
      if s.grid.(pos.row).(pos.col) == 0 then (
        new_positions := { row = pos.row; col = pos.col } :: !new_positions
      );

      get_new_positions remaining (!new_positions @ result)
  in
  let rec is_end_position_reached positions =
    match positions with
    | [] -> false
    | pos :: remaining ->
      if pos_equal pos s.end_pos then
        true
      else
        is_end_position_reached remaining
  in
  let new_positions = get_new_positions s.current_positions [] in
    { s with minutes = s.minutes + 1
    ; current_positions = new_positions
    ; is_done = is_end_position_reached new_positions
    }

let print_positions (positions: pos_t list): unit =
  let rec loop ls =
    match ls with
    | [] -> ()
    | hd :: tl -> (
      Printf.printf "(%d, %d) " hd.row hd.col;
      loop tl
    )
  in
    loop positions;
    Printf.printf "\n"
        
let print_grid (s: state_t): unit =
  for i = 0 to s.nrows - 1 do
    for j = 0 to s.ncols - 1 do
      let chr =
        match s.grid.(i).(j) with
        | 16 -> '#'
        | 1 -> '^'
        | 2 -> 'v'
        | 4 -> '<'
        | 8 -> '>'
        | 0 -> '.'
        | 3 | 5 | 6 | 9 | 10 | 12 -> '2'
        | 7 | 11 | 13 | 14 -> '3'
        | 15 -> '4'
        | _ -> failwith (Printf.sprintf "Unknown cell: %d" s.grid.(i).(j))
      in
        Printf.printf "%c" chr
    done;
    Printf.printf "\n"
  done;
  Printf.printf "\n"

let find_minimum_time (s0: state_t): state_t =
  let rec loop s iters =
    if iters > 0 && (not s.is_done) then
    begin
      let s = advance_a_minute s in
        (*print_grid s;
        print_positions s.current_positions; *)
        Printf.printf "minute: %d - is done: %b\n" s.minutes s.is_done;
        loop s (iters - 1)
    end
    else
      s
  in
    loop s0 1000

let part1 filename =
  let lines = read_file_lines filename in
  let num_rows = List.length lines in
  let num_cols = lines |> List.hd |> String.length in
  let grid: int array array =
    Array.make_matrix num_rows num_cols 0
  in
  for i = 0 to num_rows - 1 do
    let line = List.nth lines i in
    for j = 0 to num_cols - 1 do
      let piece =
        match String.get line j with
        | '#' -> wall
        | '^' -> up
        | 'v' -> down
        | '<' -> left
        | '>' -> right
        | '.' -> 0
        | _ -> failwith "Unknown char"
      in
        grid.(i).(j) <- piece
    done
  done;

  let state =
    { grid = grid
    ; next_grid = Array.make_matrix num_rows num_cols 0
    ; nrows = num_rows
    ; ncols = num_cols
    ; current_positions = [{ row = 0; col = 1 }]
    ; end_pos = { row = num_rows - 1; col = num_cols - 2 }
    ; minutes = 0
    ; is_done = false
    }
  in
    print_grid state;

  let _ = find_minimum_time state in
    ()

let part2 filename =
  let lines = read_file_lines filename in
  let num_rows = List.length lines in
  let num_cols = lines |> List.hd |> String.length in
  let grid: int array array =
    Array.make_matrix num_rows num_cols 0
  in
  for i = 0 to num_rows - 1 do
    let line = List.nth lines i in
    for j = 0 to num_cols - 1 do
      let piece =
        match String.get line j with
        | '#' -> wall
        | '^' -> up
        | 'v' -> down
        | '<' -> left
        | '>' -> right
        | '.' -> 0
        | _ -> failwith "Unknown char"
      in
        grid.(i).(j) <- piece
    done
  done;

  let state =
    { grid = grid
    ; next_grid = Array.make_matrix num_rows num_cols 0
    ; nrows = num_rows
    ; ncols = num_cols
    ; current_positions = [{ row = 0; col = 1 }]
    ; end_pos = { row = num_rows - 1; col = num_cols - 2 }
    ; minutes = 0
    ; is_done = false
    }
  in
    print_grid state;

  Printf.printf "First trip\n";
  let state = find_minimum_time state in
  let state =
    { state with end_pos = { row = 0; col = 1 }
    ; current_positions = [{ row = num_rows - 1; col = num_cols - 2 }]
    ; is_done = false
    }
  in
  Printf.printf "\nSecond trip\n";
  let state = find_minimum_time state in
  let state =
    { state with end_pos = { row = num_rows - 1; col = num_cols - 2 }
    ; current_positions = [{ row = 0; col = 1 }]
    ; is_done = false
    }
  in
  Printf.printf "\nThird trip\n";
  let _ = find_minimum_time state in
    ()


(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day24-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

