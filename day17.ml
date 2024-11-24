open Utils

let pair_snd (a, b) = b

type rock_shape_t =
  | Minus
  | Plus
  | L
  | I
  | Block

type shape_mask_t =
  { width: int
  ; height: int
  ; grid: char array array
  }

let minus_shape_mask: shape_mask_t =
  { width = 4
  ; height = 1
  ; grid = [|
      [|'#'; '#'; '#'; '#'|]
    |]
  }

let plus_shape_mask: shape_mask_t =
  { width = 3
  ; height = 3
  ; grid = [|
      [|'.'; '#'; '.'|];
      [|'#'; '#'; '#'|];
      [|'.'; '#'; '.'|]
    |]
  }

let l_shape_mask: shape_mask_t =
  { width = 3
  ; height = 3
  ; grid = [|
      [|'.'; '.'; '#'|];
      [|'.'; '.'; '#'|];
      [|'#'; '#'; '#'|]
    |]
  }

let i_shape_mask: shape_mask_t =
  { width = 1
  ; height = 4
  ; grid = [|
      [|'#'|];
      [|'#'|];
      [|'#'|];
      [|'#'|]
    |]
  }

let block_shape_mask: shape_mask_t =
  { width = 2
  ; height = 2
  ; grid = [|
      [|'#'; '#'|];
      [|'#'; '#'|]
    |]
  }

type jet_pattern_t =
  { data: string
  ; mutable pos: int
  }

let get_jet_pattern_next (p: jet_pattern_t): char =
  let current = p.pos in
  let length = String.length p.data in
    p.pos <- (current + 1) mod length;

    String.get p.data current

type state_t =
  { grid: char array array
  ; grid_height: int
  ; grid_width: int
  ; rock: rock_shape_t
  ; rock_pos: int * int
  ; min_cell_y: int
  ; rocks_per_height: (int, int) Hashtbl.t
  }

let get_next_shape (s: rock_shape_t): rock_shape_t =
  match s with
  | Minus -> Plus
  | Plus -> L
  | L -> I
  | I -> Block
  | Block -> Minus

let get_shape_mask (s: rock_shape_t): shape_mask_t =
  match s with
  | Minus -> minus_shape_mask
  | Plus -> plus_shape_mask
  | L -> l_shape_mask
  | I -> i_shape_mask
  | Block -> block_shape_mask

let get_shape_name (s: rock_shape_t): string =
  match s with
  | Minus -> "Minus"
  | Plus -> "Plus"
  | L -> "L"
  | I -> "I"
  | Block -> "Block"

let fixate_rock (s: state_t): unit =
  let (x, y) = s.rock_pos in
  let mask = get_shape_mask s.rock in
    for i = 0 to mask.height - 1 do
      for j = 0 to mask.width - 1 do
        let chr = mask.grid.(i).(j) in
        if chr == '#' then
        begin
          (* Printf.printf "| (%d, %d)" (y + i) (x + j); *)
          s.grid.(y + i).(x + j) <- '#'
        end
      done
    done

let defixate_rock (s: state_t): unit =
  let (x, y) = s.rock_pos in
  let mask = get_shape_mask s.rock in
    for i = 0 to mask.height - 1 do
      for j = 0 to mask.width - 1 do
        let chr = mask.grid.(i).(j) in
        if chr == '#' then
          s.grid.(y + i).(x + j) <- '.'
      done
    done

let print_grid (state: state_t): unit =
  let min_y = min state.min_cell_y (pair_snd state.rock_pos) in
    fixate_rock state;

    for i = min_y -1 to state.grid_height - 1 do
      for j = 0 to state.grid_width - 1 do
        Printf.printf "%c" state.grid.(i).(j)
      done;
      Printf.printf "   %d\n" i
    done;
    Printf.printf "\n";

    defixate_rock state

let detect_collision (x: int) (y: int) (s: state_t): bool =

  let mask = get_shape_mask s.rock in
  let collision = ref false in
    for i = 0 to mask.height - 1 do
      if not !collision then
        for j = 0 to mask.width - 1 do
          let chr1 = mask.grid.(i).(j) in
          let chr2 = s.grid.(y + i).(x + j) in
          if chr1 == '#' && chr2 == '#' then
          begin
            (* Printf.printf "collision at (%d, %d) \n" (y + i) (x + i); *)
            collision := true
          end;
          (*
          if y == 4040 then
            Printf.printf "'%c': (%d, %d), '%c': (%d, %d)\n" chr1 i j chr2 (y + i) (x + i)
          *)
        done
    done;

    !collision

let state_move_rock_right (s: state_t): state_t =
  let (x, y) = s.rock_pos in
  let mask = get_shape_mask s.rock in
    if (x + mask.width < s.grid_width) && not (detect_collision (x + 1) y s) then
      { s with rock_pos = (x + 1, y) }
    else
      s

let state_move_rock_left (s: state_t): state_t =
  let (x, y) = s.rock_pos in
    if (x - 1 >= 0) && not (detect_collision (x - 1) y s) then
      { s with rock_pos = (x - 1, y) }
    else
      s

let state_move_rock_down (s: state_t): bool * state_t =
  let (x, y) = s.rock_pos in
    if (y + 1 < s.grid_height) && not (detect_collision x (y + 1) s)then
      true, { s with rock_pos = (x, y + 1) }
    else
      false, s

let reajust_grid_height (s: state_t): state_t =
  if s.min_cell_y < 10 then
    let add_height = s.grid_height / 4 in
    let piece = Array.init_matrix add_height s.grid_width (fun _ _ -> '.') in
    let grid = Array.append piece s.grid in
      { s with grid = grid
      ; grid_height = s.grid_height + add_height
      ; min_cell_y = s.min_cell_y + add_height
      }
  else
    s

let run (pattern: jet_pattern_t) (s: state_t) (num_rocks: int) =
  let rec place_a_rock state0 =
    let chr = get_jet_pattern_next pattern in
      (* Printf.printf "(%c): \n" chr; *)
    let state = 
      if chr == '>' then
        state_move_rock_right state0
      else if chr == '<' then
        state_move_rock_left state0
      else
        failwith (Printf.sprintf "Unknown char %c" chr)
    in
      (* print_grid state; *)
    let (continue, state) = state_move_rock_down state in
      (* print_grid state; *)
      if continue then
        place_a_rock state
      else
      begin
        let min_cell_y = min state.min_cell_y (pair_snd state.rock_pos) in
        let state = { state with min_cell_y = min_cell_y } in
          fixate_rock state;

          state
      end
  in
  let rec loop (state: state_t) (iter: int): state_t =
    if iter < num_rocks then
    begin
      let state = place_a_rock state in
      let state = reajust_grid_height state in
      let rock = get_next_shape state.rock in
      let mask = get_shape_mask rock in
      let state =
        { state with rock = rock
        ; rock_pos = (2, state.min_cell_y - 3 - mask.height)
        }
      in
        loop state (iter + 1)
    end
    else
      let rock = get_next_shape state.rock in
      let mask = get_shape_mask rock in
      let state =
        { state with rock = rock
        ; rock_pos = (2, state.min_cell_y - 3 - mask.height)
        }
      in
        state
  in
    print_grid s;
  let s = loop s 0 in
    print_grid s;
    s

let part1 filename =
  let lines = read_file_lines filename in
  let pattern =
    { data = List.hd lines
    ; pos = 0
    }
  in
  let num_rocks = 2022 in
  let cols = 7 in
  let mtx =
    Array.init_matrix
      (2 * num_rocks)
      cols
      (fun _ _ -> '.')
  in
  let height = 2 * num_rocks in
  let state =
    { grid = mtx
    ; grid_height = height
    ; grid_width = cols
    ; rock = Minus
    ; rock_pos = (2, height - 4)
    ; min_cell_y = height - 1
    ; rocks_per_height = Hashtbl.create 2
    }
  in
  let s = run pattern state 2022 in
    Printf.printf "How tall? %d units\n" (height - s.min_cell_y)

let array_revert arr =
  let n = Array.length arr in
  let mid = n / 2 in
  for i = 0 to mid - 1 do
    let aux = arr.(n - i - 1) in
      arr.(n - i - 1) <- arr.(i);
      arr.(i) <- aux
  done

let run2 (pattern: jet_pattern_t) (s: state_t) (num_rocks: int) =
  let rec place_a_rock state0 =
    let chr = get_jet_pattern_next pattern in
      (* Printf.printf "(%c): \n" chr; *)
    let state = 
      if chr == '>' then
        state_move_rock_right state0
      else if chr == '<' then
        state_move_rock_left state0
      else
        failwith (Printf.sprintf "Unknown char %c" chr)
    in
      (* print_grid state; *)
    let (continue, state) = state_move_rock_down state in
      (* print_grid state; *)
      if continue then
        place_a_rock state
      else
      begin
        let min_cell_y = min state.min_cell_y (pair_snd state.rock_pos) in
        let state = { state with min_cell_y = min_cell_y } in
          fixate_rock state;

          state
      end
  in
  let rec loop (state: state_t) (iter: int): state_t =
    if iter < num_rocks then
    begin
      let state = place_a_rock state in
      let rock = get_next_shape state.rock in
      let mask = get_shape_mask rock in
      let state =
        { state with rock = rock
        ; rock_pos = (2, state.min_cell_y - 3 - mask.height)
        }
      in
        Hashtbl.add state.rocks_per_height state.min_cell_y (iter + 1);
        loop state (iter + 1)
    end
    else
      let rock = get_next_shape state.rock in
      let mask = get_shape_mask rock in
      let state =
        { state with rock = rock
        ; rock_pos = (2, state.min_cell_y - 3 - mask.height)
        }
      in
        state
  in
    loop s 0

let parse_bits (content: char array): int =
  let rec loop (idx: int) (result: int): int =
    if idx < 7 then
      let result =  
        if content.(idx) == '#' then
          Int.logor (Int.shift_left result 1) 1
        else
          Int.shift_left result 1
      in
        loop (idx + 1) result
    else
      result
  in
    loop 0 0

let find_match (data: int array) (n: int) (i: int) (j: int): int =
  let rec loop k =
    if k < n then
    begin
      let code0 = data.(i + k - j) in
      let code1 = data.(k) in
        if code1 != code0 then
          k
        else
          loop (k + 1)
    end
    else
      -1
  in
    loop j

let find_ciclye (state: state_t): int * int * int =
  let data =
    Array.map
      (fun row -> parse_bits row)
      state.grid
  in
    array_revert data;
  let n = state.grid_height - state.min_cell_y - 1 in
  let matches = ref [] in
  let finished = ref false in
  let i = ref 0 in
  let max_size = ref 0 in
  let max_match = ref (-1, -1, -1) in
  while !i < n do
    max_size := 0;
    max_match := (-1, -1, -1);

    for j = !i + 1 to n - 1 do
      let end_idx = find_match data n !i j in
      let size = end_idx - j in
        if size > !max_size then
        begin
          max_size := size;
          max_match := (size, state.grid_height - !i - 1, state.grid_height - j - 1)
        end;
        if !i + size > j then
          finished := true
    done;

    if !finished then
      i := n;

    if !max_size == 0 then
      incr i
    else (
      i := !i + !max_size;
      matches := !max_match :: !matches
    )
  done;
  let r = List.fold_left
    (fun bigger v ->
      let a, _, _ = bigger in
      let b, _, _ = v in
      if a > b then
        bigger
      else
        v
    )
    (-1, -1, -1)
    !matches
  in
    r

let part2 filename =
  let lines = read_file_lines filename in
  let pattern =
    { data = List.hd lines
    ; pos = 0
    }
  in
  (* maybe increase the amount for part2? in this case it wasn't needed *)
  let num_rocks = 2022 in
  let cols = 7 in
  let mtx =
    Array.init_matrix
      (2 * num_rocks)
      cols
      (fun _ _ -> '.')
  in
  let height = 2 * num_rocks in
  let state =
    { grid = mtx
    ; grid_height = height
    ; grid_width = cols
    ; rock = Minus
    ; rock_pos = (2, height - 4)
    ; min_cell_y = height - 1
    ; rocks_per_height = Hashtbl.create num_rocks
    }
  in
  let s = run2 pattern state num_rocks in
  let _, first_cycle_start, first_cycle_end = find_ciclye s in
  let size = first_cycle_start - first_cycle_end in

  (* b1 and b2 refers to the y position *)
  let b1 = Hashtbl.find s.rocks_per_height first_cycle_start in
  let b2 = Hashtbl.find s.rocks_per_height first_cycle_end in

  let rocks_amount = Int64.of_string "1000000000000" in
  let initial_rocks_amount = Int64.of_int b1 in
  let rocks_per_cycle = Int64.of_int (b2 - b1) in
  let num_of_cycles =
    Int64.div (Int64.sub rocks_amount initial_rocks_amount) rocks_per_cycle
  in
  let height_before_cycle = Int64.of_int (height - first_cycle_start - 1) in
  let height_per_cycle = Int64.of_int size in
  let ending_rocks_amount =
    Int64.sub
      (Int64.sub rocks_amount initial_rocks_amount)
      (Int64.mul rocks_per_cycle num_of_cycles)
  in
  let partial_cycle_rocks_amount = Int64.add initial_rocks_amount ending_rocks_amount in
  (* height here means the y position *)
  let partial_cycle_height =
    Hashtbl.fold
      (fun height rocks found ->
        if Option.is_none found then
          if rocks == (Int64.to_int partial_cycle_rocks_amount) then
            Some height
          else
            found
        else
          found
      )
      s.rocks_per_height
      None
  in
  let partial_cycle_height =
    match partial_cycle_height with
    | Some v -> Int64.of_int (first_cycle_start - v + 1)
    | None -> failwith "Height not found"
  in
  let total_height =
    Int64.add
      (Int64.add height_before_cycle partial_cycle_height)
      (Int64.mul num_of_cycles height_per_cycle)
  in
    Printf.printf "initial_rocks_amount: %s\n" (Int64.to_string initial_rocks_amount);
    Printf.printf "rocks_per_cycle: %s\n" (Int64.to_string rocks_per_cycle);
    Printf.printf "num_of_cycles: %s\n" (Int64.to_string num_of_cycles);
    Printf.printf "height_before_cycle: %s\n" (Int64.to_string height_before_cycle);
    Printf.printf "height_per_cycle: %s\n" (Int64.to_string height_per_cycle);
    Printf.printf "ending_rocks_amount: %s\n" (Int64.to_string ending_rocks_amount);
    Printf.printf "partial_cycle_rocks_amount: %s\n" (Int64.to_string partial_cycle_rocks_amount);
    Printf.printf "partial_cycle_height: %s\n" (Int64.to_string partial_cycle_height);
    Printf.printf "total_height: %s\n" (Int64.to_string total_height)

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day17-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

