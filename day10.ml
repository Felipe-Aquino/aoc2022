open Utils

type op_t =
  | Addx of int
  | Noop

type state_t =
  { x: int
  ; cycle: int
  ; strength: int
  ; cycle_marker: int
  ; max_cycle: int
  }

let update_cycle_strength (state: state_t): state_t =
  if state.cycle == state.cycle_marker then
    { state with
      strength = state.strength + state.x * state.cycle
    ; cycle_marker = state.cycle_marker + 40
    }
  else
    state

let int_as_str_with_pad n =
  if n >= 100 then
    string_of_int n
  else if n >= 10 then
    "0" ^ (string_of_int n)
  else
    "00" ^ (string_of_int n)

let print_op_state state op =
  match op with
  | Noop ->
    Printf.printf "%s noop | x = %d\n" (int_as_str_with_pad state.cycle) state.x
  | Addx v ->
    Printf.printf "%s addx %d | x = %d\n" (int_as_str_with_pad state.cycle) v state.x

let update_state (state: state_t) (op: op_t): state_t =
  let state = update_cycle_strength state in
    print_op_state state op;
    match op with
    | Noop -> { state with cycle = state.cycle + 1 }
    | Addx value ->
      let s = { state with cycle = state.cycle + 1 } in
      print_op_state s op;
      let s = update_cycle_strength s in
        { s with cycle = s.cycle + 1
        ; x = s.x + value
        }

let run (state: state_t) (ops: op_t list): state_t =
  let rec loop state' ops' =
    if state'.cycle < state'.max_cycle then
    begin
      match ops' with
      | [] -> state'
      | hd :: tl ->
        loop (update_state state' hd) tl
    end
    else
      state'
  in
    loop state ops

(* convert operation to string *)
let op_of_string (str: string): op_t =
  let splits = String.split_on_char ' ' str in
    match splits with
    | hd :: _ when String.equal hd "noop" ->
      Noop
    | hd :: value :: _ when String.equal hd "addx" ->
      (Addx (int_of_string value))

    | [] -> failwith "Unexpected empty line"
    | _ -> failwith ("Unexpected line: " ^ str)

let part1 filename =
  let lines = read_file_lines filename in
  let ops =
    List.fold_left
      (fun result line -> (op_of_string line) :: result)
      []
      (List.rev lines)
  in
  let state =
    { x = 1
    ; cycle = 1
    ; strength = 0
    ; cycle_marker = 20
    ; max_cycle = 250
    }
  in
  let state = run state ops in
    Printf.printf
      "x: %d, cycle: %d, strength: %d, cycle_marker: %d\n"
      state.x
      state.cycle
      state.strength
      state.cycle_marker

(* Begins part 2 solution *)
type crt_t =
  { x: int
  ; cycle: int
  ; screen: char array array
  }

let update_crt_screen(crt: crt_t): crt_t =
  let row = (crt.cycle - 1) / 40 in
  let column = (crt.cycle - 1) mod 40 in
  let is_lit = abs (crt.x - column) < 2 in
    if is_lit then
      crt.screen.(row).(column) <- '#';
    crt

let char_array_to_str (chrs: char array): string =
  String.init (Array.length chrs) (fun idx -> chrs.(idx))

let print_op_crt (crt: crt_t) (op: op_t): unit =
  let row = (crt.cycle - 1) / 40 in
  let col = (crt.cycle - 1) mod 40 in
  if row > 0 then
    Printf.printf "%s\n" (char_array_to_str crt.screen.(row - 1));

  Printf.printf
    "%s | cycle: %s, x: %d, (%d, %d)\n\n"
    (char_array_to_str crt.screen.(row))
    (int_as_str_with_pad crt.cycle)
    crt.x
    row
    col

let update_crt (crt: crt_t) (op: op_t): crt_t =
  let crt = update_crt_screen crt in
    print_op_crt crt op;
    match op with
    | Noop -> { crt with cycle = crt.cycle + 1 }
    | Addx value ->
      let crt = { crt with cycle = crt.cycle + 1 } in
        print_op_crt crt op;
      let crt = update_crt_screen crt in
        { crt with cycle = crt.cycle + 1
        ; x = crt.x + value
        }

let render (crt: crt_t) (ops: op_t list): crt_t =
  let rec loop crt' ops' =
    match ops' with
    | [] -> crt'
    | hd :: tl ->
      loop (update_crt crt' hd) tl
  in
    loop crt ops

let part2 filename =
  let lines = read_file_lines filename in
  let ops =
    List.fold_left
      (fun result line -> (op_of_string line) :: result)
      []
      (List.rev lines)
  in
  let screen = Array.init 6 (fun i -> Array.init 40 (fun _ -> '.')) in
  let crt =
    { x = 1
    ; cycle = 1
    ; screen = screen
    }
  in
  let crt = render crt ops in
    Array.iter (fun row -> Printf.printf "  %s\n" (char_array_to_str row)) crt.screen

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day10-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

