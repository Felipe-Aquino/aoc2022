open Utils

type matrix_t = int array array

type direction_t =
  | Left
  | Right
  | Top
  | Bottom

let clear_bit_from_direction (value: int) (d: direction_t): int =
  match d with
  | Left -> Int.logand value 0x7
  | Right -> Int.logand value 0xB
  | Top -> Int.logand value 0xD
  | Bottom -> Int.logand value 0xE

let char_as_int (c: char): int =
  (Char.code c) - 0x30

let read_grid (filename: string): matrix_t =
  let lines = read_file_lines filename in
  let grid =
    Array.make_matrix (List.length lines) (String.length (List.nth lines 0)) 0
  in
    List.iteri (
      fun i line ->
        String.iteri (fun j c -> grid.(i).(j) <- char_as_int c) line
    ) lines;

    grid

let make_subgrid (grid: matrix_t): matrix_t =
  Array.make_matrix (Array.length grid - 2) (Array.length grid.(0) - 2) 0x0F

let print_grid (grid: matrix_t): unit =
  Array.iter (
    fun row ->
      Array.iter (fun v -> Printf.printf "%X" v) row;
      Printf.printf "\n";
  ) grid

let update_subgrid_row_from_left (grid: matrix_t) (subgrid: matrix_t) (row: int): unit =
  let ncols = Array.length grid.(0) in
  let max_value = ref grid.(row).(0) in
    for j = 1 to ncols - 2 do
      let value = grid.(row).(j) in
        if value > !max_value then
          max_value := value
        else
          let flag_value = subgrid.(row - 1).(j - 1) in
            subgrid.(row - 1).(j - 1) <- clear_bit_from_direction flag_value Left
    done

let update_subgrid_row_from_right (grid: matrix_t) (subgrid: matrix_t) (row: int): unit =
  let ncols = Array.length grid.(0) in
  let max_value = ref grid.(row).(ncols - 1) in
    for j = 1 to ncols - 2 do
      let value = grid.(row).(ncols - j - 1) in
        if value > !max_value then
          max_value := value
        else
          let flag_value = subgrid.(row - 1).(ncols - j - 2) in
            subgrid.(row - 1).(ncols - j - 2) <- clear_bit_from_direction flag_value Right
    done

let update_subgrid_col_from_top (grid: matrix_t) (subgrid: matrix_t) (col: int): unit =
  let nrows = Array.length grid in
  let max_value = ref grid.(0).(col) in
    for i = 1 to nrows - 2 do
      let value = grid.(i).(col) in
        if value > !max_value then
          max_value := value
        else
          let flag_value = subgrid.(i - 1).(col - 1) in
            subgrid.(i - 1).(col - 1) <- clear_bit_from_direction flag_value Top
    done

let update_subgrid_col_from_bottom (grid: matrix_t) (subgrid: matrix_t) (col: int): unit =
  let nrows = Array.length grid in
  let max_value = ref grid.(nrows - 1).(col) in
    for i = 1 to nrows - 2 do
      let value = grid.(nrows - i - 1).(col) in
        if value > !max_value then
          max_value := value
        else
          let flag_value = subgrid.(nrows - i - 2).(col - 1) in
            subgrid.(nrows - i - 2).(col - 1) <- clear_bit_from_direction flag_value Bottom
    done

let non_zero_count (subgrid: matrix_t): int =
  let counter = ref 0 in
  let nrows = Array.length subgrid in
  let ncols = Array.length subgrid.(0) in
    for i = 0 to nrows - 1 do
      for j = 0 to ncols - 1 do
        if subgrid.(i).(j) != 0 then
          counter := !counter + 1
      done
    done;
    !counter

(* Subgrid contains a flag indicating if a tree is
   visible from a direction (left, right, top, bottom).
   The subgrid only represents the interior trees.
*)
let part1 filename =
  let grid = read_grid filename in
  let subgrid = make_subgrid grid in
  let nrows = Array.length grid in
  let ncols = Array.length grid.(0) in
    for i = 1 to nrows - 2 do
      update_subgrid_row_from_left grid subgrid i;
      update_subgrid_row_from_right grid subgrid i
    done;
    for j = 1 to ncols - 2 do
      update_subgrid_col_from_top grid subgrid j;
      update_subgrid_col_from_bottom grid subgrid j
    done;

  let visible_count = 2 * (nrows + ncols - 2) + non_zero_count subgrid in
    Printf.printf "total: %d\n" visible_count

    (* print_grid subgrid *)

let count_visible_trees (grid: matrix_t) (row: int) (col: int) (advance: int * int -> int * int): int =
  let nrows = Array.length grid in
  let ncols = Array.length grid.(0) in
  let value = grid.(row).(col) in

  let rec loop position count =
    let (i, j) = position in
      if 0 <= i && i < nrows && 0 <= j && j < ncols then
        if value > grid.(i).(j) then
          loop (advance position) (count + 1)
        else
          count + 1
      else
        count
  in
    loop (advance (row, col)) 0

let calculate_scenic_score (grid: matrix_t) (row: int) (col: int): int =
  (count_visible_trees grid row col (fun (i, j) -> i - 1, j)) *
  (count_visible_trees grid row col (fun (i, j) -> i + 1, j)) *
  (count_visible_trees grid row col (fun (i, j) -> i, j - 1)) *
  (count_visible_trees grid row col (fun (i, j) -> i, j + 1))

let highest_scenic_score (grid: matrix_t) (subgrid: matrix_t): int =
  let highest = ref 0 in
  let nrows = Array.length subgrid in
  let ncols = Array.length subgrid.(0) in
    for i = 0 to nrows - 1 do
      for j = 0 to ncols - 1 do
        if subgrid.(i).(j) != 0 then
          let score = calculate_scenic_score grid (i + 1) (j + 1) in
            highest := max !highest score
      done
    done;
    !highest

let part2 filename =
  let grid = read_grid filename in
  let subgrid = make_subgrid grid in
  let nrows = Array.length grid in
  let ncols = Array.length grid.(0) in
    for i = 1 to nrows - 2 do
      update_subgrid_row_from_left grid subgrid i;
      update_subgrid_row_from_right grid subgrid i
    done;
    for j = 1 to ncols - 2 do
      update_subgrid_col_from_top grid subgrid j;
      update_subgrid_col_from_bottom grid subgrid j
    done;

  let result = highest_scenic_score grid subgrid in
    Printf.printf "result: %d\n" result

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day8-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

