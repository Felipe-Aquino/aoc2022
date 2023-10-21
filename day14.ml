open Utils

type point_t =
  { x: int
  ; y: int
  }

type state_t =
  { unit_count: int
  ; start_col: int
  ; nrows: int
  ; ncols: int
  ; grid: char array array
  }

let parse_point (str: string): point_t =
  let splits = str |> String.trim |> String.split_on_char ',' in
    if List.length splits == 2 then
      let x = 0 |> List.nth splits |> int_of_string in
      let y = 1 |> List.nth splits |> int_of_string in
        { x = x
        ; y = y
        }
    else
      failwith "Could not parse point!"

let read_state (paths: point_t list list) (col_offset: int) (row_offset: int): state_t =
  let min_x = ref 500 in
  let min_y = ref 0 in
  let max_x = ref 500 in
  let max_y = ref 0 in

    for i = 0 to -1 + List.length paths do
      let path = List.nth paths i in
      for j = 0 to -1 + List.length path do
        let point = List.nth path j in
          if !min_x > point.x then
            min_x := point.x;

          if !min_y > point.y then
            min_y := point.y;

          if !max_x < point.x then
            max_x := point.x;

          if !max_y < point.y then
            max_y := point.y;
      done
    done;

    min_x := !min_x - 1 - col_offset;
    max_x := !max_x + 1 + col_offset;
    max_y := !max_y + 1 + row_offset;

    let nrows = !max_y - !min_y + 1 in
    let ncols = !max_x - !min_x + 1 in

    let grid = Array.make_matrix nrows ncols '.' in
      for i = 0 to -1 + List.length paths do
        let path = List.nth paths i in
        for j = 0 to -2 + List.length path do
          let p1 = List.nth path j in
          let p2 = List.nth path (j + 1) in
            if p1.x == p2.x then
              for k = (min p1.y p2.y) to (max p1.y p2.y) do
                grid.(k - !min_y).(p1.x - !min_x) <- '#'
              done
            else if p1.y == p2.y then
              for k = (min p1.x p2.x) to (max p1.x p2.x) do
                grid.(p1.y - !min_y).(k - !min_x) <- '#'
              done
        done
      done;

      { unit_count = 0
      ; start_col = 500 - !min_x
      ; nrows = nrows
      ; ncols = ncols
      ; grid = grid
      }

let add_one_unit_of_sand (state: state_t): state_t option =
  let rec loop (row, col) =
    let next_pos =
      if state.grid.(row + 1).(col) == '.' then
        Some (row + 1, col)
      else if state.grid.(row + 1).(col - 1) == '.' then
        Some (row + 1, col - 1)
      else if state.grid.(row + 1).(col + 1) == '.' then
        Some (row + 1, col + 1)
      else
        None
    in
      match next_pos with
      | Some (r, c) ->
        if c == 0 || c == state.ncols - 1 || r == state.nrows - 1 then
          None
        else begin
          state.grid.(row).(col) <- '.';
          state.grid.(r).(c) <- 'o';
          loop (r, c)
        end
      | None ->
        Some { state with unit_count = state.unit_count + 1 }
  in
    loop (0, state.start_col)


let part1 filename =
  let lines = read_file_lines filename in
  let paths = List.map
    (fun line ->
      line
        |> string_split_on_string " -> "
        |> List.map (fun point_str -> parse_point point_str)
    )
    lines
  in
    let state = read_state paths 0 0 in

    let rec add_sand_while_is_possible state' =
      match add_one_unit_of_sand state' with
      | Some s -> add_sand_while_is_possible s
      | None -> state'
    in
    let state = add_sand_while_is_possible state in
      Array.iter (fun row ->
        Array.iter (fun c -> Printf.printf "%c" c) row;
        Printf.printf "\n"
      ) state.grid;

      Printf.printf "unit count: %d\n" state.unit_count


let expand_grid (state: state_t): state_t =
  let new_grid = Array.make_matrix state.nrows (state.ncols + 10) '.' in
    for i = 0 to -1 + state.nrows do
      for j = 0 to -1 + state.ncols do
        new_grid.(i).(j + 5) <- state.grid.(i).(j)
      done
    done;

    for k = 0 to state.ncols + 9 do
      new_grid.(state.nrows - 1).(k) <- '#'
    done;

    { state with grid = new_grid
    ; ncols = state.ncols + 10
    ; start_col = state.start_col + 5
    }

let add_one_unit_of_sand2 (state': state_t): state_t option =
  let rec loop (row, col) state =
    let next_pos =
      if state.grid.(row + 1).(col) == '.' then
        Some (row + 1, col)
      else if state.grid.(row + 1).(col - 1) == '.' then
        Some (row + 1, col - 1)
      else if state.grid.(row + 1).(col + 1) == '.' then
        Some (row + 1, col + 1)
      else
        None
    in
      match next_pos with
      | Some (r, c) ->
        if c == 0 || c == state.ncols - 1 then
        begin
          state.grid.(row).(col) <- '.';
          state.grid.(r).(c) <- 'o';
          loop (r, c + 5) (expand_grid state)
        end
        else if r == state.nrows - 1 then
          None
        else begin
          state.grid.(row).(col) <- '.';
          state.grid.(r).(c) <- 'o';
          loop (r, c) state
        end
      | None ->
        if row == 0 && col == state.start_col then
          None
        else
          Some { state with unit_count = state.unit_count + 1 }
  in
    loop (0, state'.start_col) state'

let part2 filename =
  let lines = read_file_lines filename in
  let paths = List.map
    (fun line ->
      line
        |> string_split_on_string " -> "
        |> List.map (fun point_str -> parse_point point_str)
    )
    lines
  in
    let state = read_state paths 4 1 in
      for k = 0 to state.ncols - 1 do
        state.grid.(state.nrows - 1).(k) <- '#'
      done;

    let rec add_sand_while_is_possible state' =
      match add_one_unit_of_sand2 state' with
      | Some s -> add_sand_while_is_possible s
      | None -> state'
    in
    let state = add_sand_while_is_possible state in
      (*
      Array.iter (fun row ->
        Array.iter (fun c -> Printf.printf "%c" c) row;
        Printf.printf "\n"
      ) state.grid;
      *)

      Printf.printf "unit count: %d\n" (state.unit_count + 1)


(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day14-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

