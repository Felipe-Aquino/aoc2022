open Utils

type bounds_t =
  { min_col: int
  ; min_row: int
  ; max_col: int
  ; max_row: int
  }

type elf_t =
  { id: int
  ; mutable row: int
  ; mutable col: int
  ; mutable start_dir: int
  ; mutable proposed_dir: int option
  }

let read_elves (i: int) (line: string) =
  let n = String.length line in
  let rec loop (j: int) (result: elf_t list) =
    if j < n then
    begin
      let c = String.get line j in
        if c == '#' then
          let elf =
            { id = 0
            ; row = i + 11
            ; col = j + 11
            ; start_dir = 0
            ; proposed_dir = None
            }
          in
           loop (j + 1) (elf :: result)
        else
           loop (j + 1) result
    end
    else
      result
  in
    loop 0 []

let first_allowed_dir (grid: int array array) (row: int) (col: int) (dir0: int): int option =
  let rec loop iter dir =
    if iter < 4 then
      let can_go =
        match dir with
        | 0 ->
          grid.(row - 1).(col)     <= 0 && (* N  *)
          grid.(row - 1).(col - 1) <= 0 && (* NW *)
          grid.(row - 1).(col + 1) <= 0    (* NE *)
        | 1 ->
          grid.(row + 1).(col)     <= 0 && (* S  *)
          grid.(row + 1).(col - 1) <= 0 && (* SW *)
          grid.(row + 1).(col + 1) <= 0    (* SE *)
        | 2 ->
          grid.(row - 1).(col - 1) <= 0 && (* NW *)
          grid.(row).(col - 1)     <= 0 && (* W  *)
          grid.(row + 1).(col - 1) <= 0    (* SW *)
        | 3 ->
          grid.(row - 1).(col + 1) <= 0 && (* NE *)
          grid.(row).(col + 1)     <= 0 && (* E  *)
          grid.(row + 1).(col + 1) <= 0    (* SE *)
        | _ ->
          failwith "Direction should not exist"
      in
      if can_go then
        Some dir
      else
        loop (iter + 1) ((dir + 1) mod 4)
    else
      None
  in
    loop 0 dir0

let process_round (grid: int array array) (elves: elf_t array): int =
  let n = Array.length elves in
  let moves_count = ref 0 in
  let rec check_moves idx =
    if idx < n then
    begin
      let elf = elves.(idx) in
      let should_try_move =
        grid.(elf.row - 1).(elf.col) > 0     || (* N  *)
        grid.(elf.row - 1).(elf.col - 1) > 0 || (* NW *)
        grid.(elf.row - 1).(elf.col + 1) > 0 || (* NE *)
        grid.(elf.row + 1).(elf.col) > 0     || (* S  *)
        grid.(elf.row + 1).(elf.col - 1) > 0 || (* SW *)
        grid.(elf.row + 1).(elf.col + 1) > 0 || (* SE *)
        grid.(elf.row).(elf.col - 1) > 0     || (* W  *)
        grid.(elf.row).(elf.col + 1) > 0        (* E  *)
      in
      if should_try_move then
      begin
        let available_dir = first_allowed_dir grid elf.row elf.col elf.start_dir in
          match available_dir with
          | None ->
            elf.start_dir <- (elf.start_dir + 1) mod 4;
            elf.proposed_dir <- None
          | Some dir -> (
            elf.start_dir <- (elf.start_dir + 1) mod 4;
            elf.proposed_dir <- Some dir;
            match dir with
            | 0 ->
              grid.(elf.row - 1).(elf.col) <- grid.(elf.row - 1).(elf.col) - 1 
            | 1 ->
              grid.(elf.row + 1).(elf.col) <- grid.(elf.row + 1).(elf.col) - 1 
            | 2 ->
              grid.(elf.row).(elf.col - 1) <- grid.(elf.row).(elf.col - 1) - 1 
            | 3 ->
              grid.(elf.row).(elf.col + 1) <- grid.(elf.row).(elf.col + 1) - 1 
            | _ ->
              failwith "Direction should not exist"
          )
      end
      else (
        elf.proposed_dir <- None;
        elf.start_dir <- (elf.start_dir + 1) mod 4
      );
      check_moves (idx + 1)
    end
  in
  let rec move_elves idx =
    if idx < n then
    begin
      let elf = elves.(idx) in
        (match elf.proposed_dir with
         | None -> ()
         | Some dir ->
           match dir with
           | 0 ->
             if grid.(elf.row - 1).(elf.col) == -1 then (
               incr moves_count;
               elf.proposed_dir <- None;
               grid.(elf.row).(elf.col) <- 0;
               grid.(elf.row - 1).(elf.col) <- elf.id;
               elf.row <- elf.row - 1
             )
           | 1 ->
             if grid.(elf.row + 1).(elf.col) == -1 then (
               incr moves_count;
               elf.proposed_dir <- None;
               grid.(elf.row).(elf.col) <- 0;
               grid.(elf.row + 1).(elf.col) <- elf.id;
               elf.row <- elf.row + 1
             )
           | 2 ->
             if grid.(elf.row).(elf.col - 1) == -1 then (
               incr moves_count;
               elf.proposed_dir <- None;
               grid.(elf.row).(elf.col) <- 0;
               grid.(elf.row).(elf.col - 1) <- elf.id;
               elf.col <- elf.col - 1
             )
           | 3 ->
             if grid.(elf.row).(elf.col + 1) == -1 then (
               incr moves_count;
               elf.proposed_dir <- None;
               grid.(elf.row).(elf.col) <- 0;
               grid.(elf.row).(elf.col + 1) <- elf.id;
               elf.col <- elf.col + 1
             )
           | _ ->
             failwith "Direction should not exist"
        );
        move_elves (idx + 1)
    end
  in
  let rec clear_grid_negatives idx =
    if idx < n then
    begin
      let elf = elves.(idx) in
        (match elf.proposed_dir with
         | None -> ()
         | Some dir ->
           match dir with
           | 0 ->
             if grid.(elf.row - 1).(elf.col) < 0 then
               grid.(elf.row - 1).(elf.col) <- 0
           | 1 ->
             if grid.(elf.row + 1).(elf.col) < 0 then
               grid.(elf.row + 1).(elf.col) <- 0
           | 2 ->
             if grid.(elf.row).(elf.col - 1) < 0 then
               grid.(elf.row).(elf.col - 1) <- 0
           | 3 ->
             if grid.(elf.row).(elf.col + 1) < 0 then
               grid.(elf.row).(elf.col + 1) <- 0
           | _ ->
             failwith "Direction should not exist"
        );
        clear_grid_negatives (idx + 1)
    end
  in
    check_moves 0;
    move_elves 0;
    clear_grid_negatives 0;

    !moves_count

let print_grid (grid: int array array) (nrows: int) (ncols: int): unit =
  for i = 0 to nrows - 1 do
    for j = 0 to ncols - 1 do
      let v = grid.(i).(j) in
      if v == 0 then
        Printf.printf "."
      else if v < 0 then
        Printf.printf "?"
      else
        (* Printf.printf "%c" (Char.chr (v - 10 + 65)) *)
        Printf.printf "#"
    done;
    Printf.printf "\n"
  done;
  Printf.printf "\n"

let process_10_rounds grid elves num_rows num_cols =
  let rec loop iters =
    if iters < 10 then
    begin
      let _ = process_round grid elves in
        (* print_grid grid (num_rows + 22) (num_cols + 22); *)
        loop (iters + 1)
    end
  in
    loop 0

let get_empty_tiles_count (elves: elf_t array): int =
  let n = Array.length elves in
  let bounds =
    { min_col = Int.max_int
    ; min_row = Int.max_int
    ; max_col = Int.min_int
    ; max_row = Int.min_int
    }
  in
  let bounds =
    Array.fold_left
      (fun b elf ->
        { min_col = min b.min_col elf.col
        ; min_row = min b.min_row elf.row
        ; max_col = max b.max_col elf.col
        ; max_row = max b.max_row elf.row
        }
      )
      bounds
      elves
  in
  let row_count = bounds.max_row - bounds.min_row + 1 in
  let col_count = bounds.max_col - bounds.min_col + 1 in
    row_count * col_count - n

let part1 filename =
  let lines = read_file_lines filename in
  let num_rows = List.length lines in
  let num_cols = lines |> List.hd |> String.length in
  let grid: int array array =
    Array.make_matrix (num_rows + 22) (num_cols + 22) 0
  in
  let elves =
    lines
    |> List.mapi read_elves
    |> List.flatten
    |> Array.of_list
  in
  let elves =
    Array.mapi (fun i elf -> { elf with id = (i + 10) }) elves
  in
    Array.iter (fun elf -> grid.(elf.row).(elf.col) <- elf.id) elves;

    (* print_grid grid (num_rows + 22) (num_cols + 22); *)
    process_10_rounds grid elves num_rows num_cols;

    Printf.printf "Number of empty ground tile is %d\n" (get_empty_tiles_count elves)

let print_grid_with_bounds (grid: int array array) (elves: elf_t array): unit =
  let bounds =
    { min_col = Int.max_int
    ; min_row = Int.max_int
    ; max_col = Int.min_int
    ; max_row = Int.min_int
    }
  in
  let bounds =
    Array.fold_left
      (fun b elf ->
        { min_col = min b.min_col elf.col
        ; min_row = min b.min_row elf.row
        ; max_col = max b.max_col elf.col
        ; max_row = max b.max_row elf.row
        }
      )
      bounds
      elves
  in
  for i = bounds.min_row to bounds.max_row do
    for j = bounds.min_col to bounds.max_col do
      let v = grid.(i).(j) in
      if v == 0 then
        Printf.printf "."
      else if v < 0 then
        Printf.printf "?"
      else
        (* Printf.printf "%c" (Char.chr (v - 10 + 65)) *)
        Printf.printf "#"
    done;
    Printf.printf "\n"
  done;
  Printf.printf "\n"

let read_elves2 (i: int) (line: string) =
  let n = String.length line in
  let rec loop (j: int) (result: elf_t list) =
    if j < n then
    begin
      let c = String.get line j in
        if c == '#' then
          let elf =
            { id = 0
            ; row = i
            ; col = j
            ; start_dir = 0
            ; proposed_dir = None
            }
          in
           loop (j + 1) (elf :: result)
        else
           loop (j + 1) result
    end
    else
      result
  in
    loop 0 []

let process_until_nobody_moves grid elves =
  let rec loop iters =
    if iters < 10000 then
    begin
      let move_count = process_round grid elves in
        if move_count > 0 then
          (* print_grid grid (num_rows + 22) (num_cols + 22); *)
          loop (iters + 1)
        else
          iters
    end
    else
      iters
  in
    loop 1

let part2 filename =
  let lines = read_file_lines filename in
  let num_rows = List.length lines in
  let num_cols = lines |> List.hd |> String.length in
  let elves =
    lines
    |> List.mapi read_elves2
    |> List.flatten
    |> Array.of_list
  in
  let elves_count = Array.length elves in
  let num_rows = num_rows + 2 * (elves_count + 1) in
  let num_cols = num_cols + 2 * (elves_count + 1) in
  let grid: int array array =
    Array.make_matrix num_rows num_cols 0
  in
  let elves =
    Array.mapi
      (fun i elf ->
        { elf with id = (i + 10)
        ; row = elf.row + (elves_count + 1)
        ; col = elf.col + (elves_count + 1)
        }
      )
      elves
  in
    Array.iter (fun elf -> grid.(elf.row).(elf.col) <- elf.id) elves;

  let round_count = process_until_nobody_moves grid elves in
    (*print_grid_with_bounds grid elves;*)
    Printf.printf "The number of the first round where no Elf moves is %d\n" round_count

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day23-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

