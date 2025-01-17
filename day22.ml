open Utils

type vec2_t =
  { x: int
  ; y: int
  }

type action_t =
  | Right
  | Left
  | Step of int

type state_t =
  { board: char array array
  ; num_rows: int
  ; num_cols: int
  ; pos: vec2_t
  ; dir: vec2_t
  ; path: action_t list
  ; cube_wraps: (vec2_t, (vec2_t * vec2_t)) Hashtbl.t 
  }

let vec2_add (v1: vec2_t) (v2: vec2_t): vec2_t =
  { x = v1.x + v2.x
  ; y = v1.y + v2.y
  }

let is_digit (c: char): bool = c >= '0' && c <= '9'

let parse_path (path: string): action_t list =
  let n = String.length path in
  let rec loop idx result =
    if idx < n then
    begin
      let c = String.get path idx in
      if c == 'R' then
        loop (idx + 1) (Right :: result)
      else if c == 'L' then
        loop (idx + 1) (Left :: result)
      else
      begin
        let count = ref 0 in
        let quit = ref false in
        while not !quit do
          if (idx + !count) < n then (
            let c = String.get path (idx + !count) in
              if is_digit c then
                incr count
              else
                quit := true
          ) else
            quit := true
        done;

        let value =
          int_of_string (String.sub path idx !count)
        in
          loop (idx + !count) ((Step value) :: result)
      end
    end
    else
      result
  in
    List.rev (loop 0 [])

let turn_clockwise (v: vec2_t): vec2_t =
  { x = -v.y
  ; y = v.x
  }

let turn_counterclockwise (v: vec2_t): vec2_t =
  { x = v.y
  ; y = -v.x
  }

let get_board_tile (state: state_t) (pos: vec2_t): char option =
  if pos.x < 0 || pos.x >= state.num_cols then
    None
  else if pos.y < 0 || pos.y >= state.num_rows then
    None
  else
    Some state.board.(pos.y).(pos.x)

let wrap_position (state: state_t) (pos: vec2_t): vec2_t =
  if state.dir.x == 1 then
    { pos with x = 0 }
  else if state.dir.x == -1 then
    { pos with x = state.num_cols - 1 }
  else if state.dir.y == 1 then
    { pos with y = 0 }
  else if state.dir.y == -1 then
    { pos with y = state.num_rows - 1 }
  else
    pos

let rec find_non_empty (state: state_t) (start: vec2_t) (dir: vec2_t): vec2_t =
  let c = state.board.(start.y).(start.x) in
    if c == ' ' then
      find_non_empty state (vec2_add start dir) dir
    else
      start

let dir_number (dir: vec2_t): int =
  if dir.x == 1 then
    0
  else if dir.x == -1 then
    2
  else if dir.y == 1 then
    1
  else
    3

let write_dir_char (state: state_t) (pos: vec2_t): unit =
  if state.dir.x == 1 then
    state.board.(pos.y).(pos.x) <- '>'
  else if state.dir.x == -1 then
    state.board.(pos.y).(pos.x) <- '<'
  else if state.dir.y == 1 then
    state.board.(pos.y).(pos.x) <- 'v'
  else
    state.board.(pos.y).(pos.x) <- '^'

let rec go_n_steps (state: state_t) (n: int): state_t =
  if n > 0 then
    begin
    let next_pos = vec2_add state.pos state.dir in
    let tile = get_board_tile state next_pos in
    match tile with
    | None ->
      let next_pos = wrap_position state next_pos in
      let next_pos = find_non_empty state next_pos state.dir in
      let c = state.board.(next_pos.y).(next_pos.x) in
        if c == '#' then
          state
        else (
          write_dir_char state next_pos;
          go_n_steps { state with pos = next_pos } (n - 1)
        )
    | Some c when c == ' ' ->
      let next_pos = wrap_position state next_pos in
      let next_pos = find_non_empty state next_pos state.dir in
      let c = state.board.(next_pos.y).(next_pos.x) in
        if c == '#' then
          state
        else (
          write_dir_char state next_pos;
          go_n_steps { state with pos = next_pos } (n - 1)
        )
    | Some c when c == '#' -> state
    | _ -> (
        write_dir_char state next_pos;
        go_n_steps { state with pos = next_pos } (n - 1)
    )
  end
  else
    state

let run (state0: state_t): state_t =
  let rec loop state iters =
    if List.length state.path > 0 && iters > 0 then
    begin
      let action = List.hd state.path in
      let others = List.tl state.path in
      match action with
      | Right ->
        let state =
          { state with path = others
          ; dir = turn_clockwise state.dir
          }
        in (
          write_dir_char state state.pos;
          loop state (iters - 1)
        )
      | Left ->
        let state =
          { state with path = others
          ; dir = turn_counterclockwise state.dir
          }
        in (
          write_dir_char state state.pos;
          loop state (iters - 1)
        )
      | Step count ->
        let state = go_n_steps state count in
        let state = { state with path = others } in
          loop state (iters - 1)
    end
    else
      state
  in
    write_dir_char state0 state0.pos;
    loop state0 100000

let part1 filename =
  let lines = read_file_lines filename in
  let board_lines = take ((List.length lines) - 2) lines in
  let path = List.nth lines ((List.length lines) - 1) in

  let num_rows = List.length board_lines in
  let num_cols =
    List.fold_left
      (fun total line -> max total (String.length line))
      0
      board_lines
  in
  let board = Array.make_matrix num_rows num_cols ' ' in
    List.iteri
      (fun i line -> String.iteri (fun j c -> board.(i).(j) <- c) line)
      board_lines;

  let starting_col =
    board.(0)
    |> Array.find_index (fun a -> a == '.')
    |> Option.get
  in
  let state =
    { board = board
    ; num_rows = num_rows
    ; num_cols = num_cols
    ; pos = { x = starting_col; y = 0 }
    ; dir = { x = 1; y = 0 }
    ; path = parse_path path
    ; cube_wraps = Hashtbl.create 1
    }
  in
    let state = run state in
    let facing = dir_number state.dir in
    let password = 1000 * (state.pos.y + 1) + 4 * (state.pos.x + 1) + facing in
      (*Printf.printf "path: %s\n" path; *)
      Printf.printf "pos: %d, %d\n" state.pos.x state.pos.y;
      Printf.printf "facing: %d\n" facing;
      Printf.printf "password: %d\n" password
      
      (*Array.iter
        (fun row ->
          Array.iter (fun c -> Printf.printf "%c" c) row;
          Printf.printf "\n"
        )
        board
      *)


(* Cube folding map for the example input layout

     0123456789ABCDEF
               1
               v
0            1111
1         2> 1111 <3
2     1  2   1111
3     v  v   1111
4    222233334444
5 4> 222233334444 <7
6    222233334444  7
7    222233334444  v
8     ^  ^   55556666
9     5  6   55556666 <3
10        6> 55556666
11           55556666
              ^   ^
              5   4

Face 1.
dir = (-1, 0) -> (0, 1)
(8, 0..3) -> (4..7, 4)

dir = (1, 0) -> (-1, 0)
(11, 0..3) -> (15, 11..8)

dir = (0, -1) -> (0, 1)
(8..11, 0) -> (3..0, 4)

Face 2.
dir = (0, -1) -> (0, 1)
(3..0, 4) -> (8..11, 0) 

dir = (0, 1) -> (0, -1)
(3..0, 7) -> (8..11, 11) 

dir = (-1, 0) -> (0, -1)
(0, 4..7) -> (15..12, 11)

Face 3.
dir = (0, -1) -> (1, 0)
(4..7, 4) -> (8, 0..3)

dir = (0, 1) -> (1, 0)
(4..7, 7) -> (8, 11..8)

Face 4.
dir = (1, 0) -> (0, 1)
(11, 4..7) -> (15..12, 8)

Face 5.
dir = (-1, 0) -> (0, -1)
(8, 11..8) -> (4..7, 7)

dir = (0, 1) -> (0, -1)
(8..11, 11) -> (3..0, 7) 

Face 6.
dir = (0, -1) -> (-1, 0)
(15..12, 8) -> (11, 4..7)

dir = (0, 1) -> (1, 0)
(15..12, 11) -> (0, 4..7)

dir = (1, 0) -> (-1, 0)
(11, 0..3) -> (15, 11..8)
*)
let create_cube_wraps (cube_size: int): (vec2_t, (vec2_t * vec2_t)) Hashtbl.t =
  let wraps = Hashtbl.create (14 * cube_size) in
  let n = cube_size in

  let down = { x = 0; y = 1 } in
  let up = { x = 0; y = -1 } in
  let left = { x = -1; y = 0 } in
  let right = { x = 1; y = 0 } in

  let rec loop i =
    if i < n then
    begin
      (* Face 1 *)
      Hashtbl.add wraps {x = 2*n; y = i} ({x = n + i; y = n}, down);
      Hashtbl.add wraps {x = 3*n-1; y = i} ({x = 4*n-1; y = 3*n-1 - i}, left);
      Hashtbl.add wraps {x = 2*n + i; y = 0} ({x = n-1 - i; y = n}, down);

      (* Face 2 *)
      Hashtbl.add wraps {x = n-1 - i; y = n} ({x = 2*n + i; y = 0}, down);
      Hashtbl.add wraps {x = n-1 - i; y = 2*n-1} ({x = 2*n + i; y = 3*n-1}, up);
      Hashtbl.add wraps {x = 0; y = n + i} ({x = 4*n-1 - i; y = 3*n-1}, up);

      (* Face 3 *)
      Hashtbl.add wraps {x = n + i; y = n} ({x = 2*n; y = i}, right);
      Hashtbl.add wraps {x = n + i; y = 2*n-1} ({x = n; y = 3*n-1 - i}, right);

      (* Face 4 *)
      Hashtbl.add wraps {x = 3*n-1; y = n + i} ({x = 4*n-1 - i; y = 2*n}, down);

      (* Face 5 *)
      Hashtbl.add wraps {x = 2*n; y = 3*n-1 - i} ({x = n + i; y = 2*n-1}, up);
      Hashtbl.add wraps {x = 2*n + i; y = 3*n-1} ({x = n-1 - i; y = 2*n-1}, up);

      (* Face 6 *)
      Hashtbl.add wraps {x = 4*n-1 - i; y = 2*n} ({x = 3*n-1; y = n + i}, left);
      Hashtbl.add wraps {x = 4*n-1 - i; y = 3*n-1} ({x = 0; y = n + i}, right);
      Hashtbl.add wraps {x = 3*n-1; y = i} ({x = 4*n-1; y = 3*n-1 - i}, left);

      loop (i + 1)
    end
  in
    loop 0;
    wraps

(* Cube folding map for the given input layout in 4x4

      0123456789ABCDEF
           1    2
           v    v
0         11112222
1         11112222
2      5> 11112222 <4
3         11112222
4         3333  ^
5      6> 3333  3
6      6  3333   
7      v  3333 <3
8     55554444    
9     55554444 <4 
10 5> 55554444    
11    55554444    
12    6666  ^
13 1> 6666  7
14    6666
15    6666 <7
       ^
       2

Face 1.
dir = (0, -1) -> (1, 0)
(4..7, 0) -> (0, 12..15)

dir = (-1, 0) -> (1, 0)
(4, 0..3) -> (0, 11..8)

Face 2.
dir = (0, -1) -> (0, -1)
(8..11, 0) -> (0..3, 15)

dir = (0, 1) -> (-1, 0)
(8..11, 3) -> (7, 4..7)

dir = (1, 0) -> (-1, 0)
(11, 0..3) -> (7, 11..8)

Face 3.
dir = (1, 0) -> (0, -1)
(7, 4..7) -> (8..11, 3)

dir = (-1, 0) -> (0, 1)
(4, 4..7) -> (0..3, 8)

Face 4.
dir = (1, 0) -> (-1, 0)
(7, 11..8) -> (11, 0..3)

dir = (0, 1) -> (-1, 0)
(4..7, 11) -> (3, 12..15)

Face 5.
dir = (-1, 0) -> (1, 0)
(0, 11..8) -> (4, 0..3)

dir = (0, -1) -> (1, 0)
(0..3, 8) -> (4, 4..7)

Face 6.
dir = (-1, 0) -> (0 ,1)
(0, 12..15) -> (4..7, 0)

dir = (1, 0) -> (0, -1)
(3, 12..15) -> (4..7, 11)

dir = (0, 1) -> (0, 1)
(0..3, 15) -> (8..11, 0)
 *)
let create_cube_wraps2 (cube_size: int): (vec2_t, (vec2_t * vec2_t)) Hashtbl.t =
  let wraps = Hashtbl.create (14 * cube_size) in
  let n = cube_size in

  let down = { x = 0; y = 1 } in
  let up = { x = 0; y = -1 } in
  let left = { x = -1; y = 0 } in
  let right = { x = 1; y = 0 } in

  let rec loop i =
    if i < n then
    begin
      (* Face 1 *)
      Hashtbl.add wraps {x = n + i; y = 0} ({x = 0; y = 3*n + i}, right);
      Hashtbl.add wraps {x = n; y = i} ({x = 0; y = 3*n-1 - i}, right);

      (* Face 2 *)
      Hashtbl.add wraps {x = 2*n + i; y = 0} ({x = i; y = 4*n-1}, up);
      Hashtbl.add wraps {x = 2*n + i; y = n-1} ({x = 2*n - 1; y = n + i}, left);
      Hashtbl.add wraps {x = 3*n-1; y = i} ({x = 2*n-1; y = 3*n-1 - i}, left);

      (* Face 3 *)
      Hashtbl.add wraps {x = 2*n-1; y = n + i} ({x = 2*n + i; y = n-1}, up);
      Hashtbl.add wraps {x = n; y = n + i} ({x = i; y = 2*n}, down);

      (* Face 4 *)
      Hashtbl.add wraps {x = 2*n-1; y = 3*n-1 - i} ({x = 3*n-1; y = i}, left);
      Hashtbl.add wraps {x = n + i; y = 3*n-1} ({x = n-1; y = 3*n + i}, left);

      (* Face 5 *)
      Hashtbl.add wraps {x = 0; y = 3*n-1 - i} ({x = n; y = i}, right);
      Hashtbl.add wraps {x = i; y = 2*n} ({x = n; y = n + i}, right);

      (* Face 6 *)
      Hashtbl.add wraps {x = 0; y = 3*n + i} ({x = n + i; y = 0}, down);
      Hashtbl.add wraps {x = n-1; y = 3*n + i} ({x = n + i; y = 3*n-1}, up);
      Hashtbl.add wraps {x = i; y = 4*n-1} ({x = 2*n + i; y = 0}, down);

      loop (i + 1)
    end
  in
    loop 0;
    wraps

let rec go_n_steps2 (state: state_t) (n: int): state_t =
  if n > 0 then
  begin
    let next_pos = vec2_add state.pos state.dir in
    let tile = get_board_tile state next_pos in
    match tile with
    | None ->
      let next_pos, new_dir = Hashtbl.find state.cube_wraps state.pos in
      let next_pos = find_non_empty state next_pos new_dir in
      let c = state.board.(next_pos.y).(next_pos.x) in
        if c == '#' then
          state
        else (
          let state = { state with dir = new_dir } in
            write_dir_char state next_pos;
            go_n_steps2 { state with pos = next_pos } (n - 1)
        )
    | Some c when c == ' ' ->
      let next_pos, new_dir = Hashtbl.find state.cube_wraps state.pos in
      let next_pos = find_non_empty state next_pos new_dir in
      let c = state.board.(next_pos.y).(next_pos.x) in
        if c == '#' then
          state
        else (
          let state = { state with dir = new_dir } in
            write_dir_char state next_pos;
            go_n_steps2 { state with pos = next_pos } (n - 1)
        )
    | Some c when c == '#' -> state
    | _ -> (
      write_dir_char state next_pos;
      go_n_steps2 { state with pos = next_pos } (n - 1)
    )
  end
  else
    state

let run2 (state0: state_t): state_t =
  let rec loop state iters =
    if List.length state.path > 0 && iters > 0 then
    begin
      let action = List.hd state.path in
      let others = List.tl state.path in
      match action with
      | Right ->
        let state =
          { state with path = others
          ; dir = turn_clockwise state.dir
          }
        in (
          write_dir_char state state.pos;
          loop state (iters - 1)
        )
      | Left ->
        let state =
          { state with path = others
          ; dir = turn_counterclockwise state.dir
          }
        in (
          write_dir_char state state.pos;
          loop state (iters - 1)
        )
      | Step count ->
        let state = go_n_steps2 state count in
        let state = { state with path = others } in
          loop state (iters - 1)
    end
    else
      state
  in
    write_dir_char state0 state0.pos;
    loop state0 100000

let part2 filename =
  let lines = read_file_lines filename in
  let board_lines = take ((List.length lines) - 2) lines in
  let path = List.nth lines ((List.length lines) - 1) in

  let num_rows = List.length board_lines in
  let num_cols =
    List.fold_left
      (fun total line -> max total (String.length line))
      0
      board_lines
  in
  let board = Array.make_matrix num_rows num_cols ' ' in
    List.iteri
      (fun i line -> String.iteri (fun j c -> board.(i).(j) <- c) line)
      board_lines;

  let starting_col =
    board.(0)
    |> Array.find_index (fun a -> a == '.')
    |> Option.get
  in
  let cube_size =
    if String.equal filename "./inputs/day22-example.txt" then
      4
    else
      50
  in
  let cube_wraps =
    if String.equal filename "./inputs/day22-example.txt" then
      create_cube_wraps cube_size
    else
      create_cube_wraps2 cube_size
  in
  let state =
    { board = board
    ; num_rows = num_rows
    ; num_cols = num_cols
    ; pos = { x = starting_col; y = 0 }
    ; dir = { x = 1; y = 0 }
    ; path = parse_path path
    ; cube_wraps = cube_wraps
    }
  in
    let state = run2 state in
    let facing = dir_number state.dir in
    let password = 1000 * (state.pos.y + 1) + 4 * (state.pos.x + 1) + facing in
      (*Array.iter
        (fun row ->
          Array.iter (fun c -> Printf.printf "%c" c) row;
          Printf.printf "\n"
        )
        board;
      *)
      Printf.printf "pos: %d, %d\n" state.pos.x state.pos.y;
      Printf.printf "facing: %d\n" facing;
      Printf.printf "password: %d\n" password

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day22-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

