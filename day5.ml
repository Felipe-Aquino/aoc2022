open Utils

(* stack operations *)

let push (value: 'a) (stack: 'a list): 'a list = value :: stack

let pop (stack: 'a list): 'a list =
  match stack with
  | [] -> []
  | head :: tail -> tail

let top (stack: 'a list): 'a =
  match stack with
  | [] -> failwith "Empty stack"
  | head :: tail -> head


(* crates *)

let parse_one_crate (input: string) (pos: int): (int * char option) =
  let value =
    if String.get input pos = '[' then
      Some (String.get input (pos + 1))
    else
      None
  in
    (pos + 4, value)

let parse_crates (input: string): (char option) list =
  let rec loop (pos: int): (char option) list =
    if String.length input > pos then
      let (new_pos, value) = parse_one_crate input pos in
        value :: (loop new_pos)
    else
      []
  in
    loop 0

let string_of_crate_opt = function
  | None -> "(nil)"
  | Some chr -> Printf.sprintf "%c" chr

let read_stacks (lines: string list): ((char list) array) =
  let crate_lines = List.map parse_crates lines in
  let stack_count = List.length (List.nth crate_lines 0)  in
    Array.init stack_count (fun i ->
      crate_lines
        |> List.map (fun crates -> List.nth crates i)
        |> List.filter_map _id
    )

(* instructions *)

type instruction_t =
  { move_amount: int
  ; src: int
  ; dest: int
  }

let string_of_instruction (inst: instruction_t): string =
  Printf.sprintf "move %d from %d to %d" inst.move_amount inst.src inst.dest

let parse_one_instruction (input: string): instruction_t =
  let words = String.split_on_char ' ' input in
  let get_as_int at = at |> List.nth words |> int_of_string in
    { move_amount = get_as_int 1
    ; src = get_as_int 3
    ; dest = get_as_int 5
    }

let rec parse_instructions (lines: string list): instruction_t list =
  match lines with
  | [] -> []
  | hd :: tl -> (parse_one_instruction hd) :: (parse_instructions tl)

let run_instruction (stacks: char list array) (inst: instruction_t): unit =
  let dest = inst.dest - 1 in
  let src = inst.src - 1 in
    for _ = 0 to inst.move_amount - 1 do
      stacks.(dest) <- push (top stacks.(src)) stacks.(dest);
      stacks.(src) <- pop stacks.(src)
    done

let part1 filename =
  let lines = read_file_lines filename in

  let splited = split_by (fun _ line -> String.length line == 0) lines in
  let crates_lines = (* List.nth splited 0 *)
    let splited0 = List.nth splited 0 in take (List.length splited0 - 2) splited0
  in
  let stacks = read_stacks crates_lines in

  let instructions_lines = List.nth splited 1 in
  let instructions = parse_instructions instructions_lines in

    List.iter (fun inst -> run_instruction stacks inst) instructions;

    Printf.printf "%s\n" (Array.get stacks 0 |> list_to_string (fun c -> String.make 1 c));
    Printf.printf "%s\n" (Array.get stacks 1 |> list_to_string (fun c -> String.make 1 c));
    Printf.printf "%s\n" (Array.get stacks 2 |> list_to_string (fun c -> String.make 1 c));
    Printf.printf "%s\n" (List.nth instructions 1 |> string_of_instruction);

    Printf.printf "\nresult: %s\n" (
      Array.fold_left (fun result stack -> result ^ (String.make 1 (top stack))) "" stacks
    )


let run_instruction_crate_mover9001 (stacks: char list array) (inst: instruction_t): unit =
  let dest = inst.dest - 1 in
  let src = inst.src - 1 in
  let amount = inst.move_amount in
  let moved_crates = take amount (stacks.(src)) in
    stacks.(dest) <- moved_crates @ stacks.(dest);
    for _ = 0 to amount - 1 do
      stacks.(src) <- pop stacks.(src)
    done

let part2 filename =
  let lines = read_file_lines filename in

  let splited = split_by (fun _ line -> String.length line == 0) lines in
  let crates_lines = (* List.nth splited 0 *)
    let splited0 = List.nth splited 0 in take (List.length splited0 - 2) splited0
  in
  let stacks = read_stacks crates_lines in

  let instructions_lines = List.nth splited 1 in
  let instructions = parse_instructions instructions_lines in

    List.iter (fun inst -> run_instruction_crate_mover9001 stacks inst) instructions;

    Printf.printf "%s\n" (Array.get stacks 0 |> list_to_string (fun c -> String.make 1 c));
    Printf.printf "%s\n" (Array.get stacks 1 |> list_to_string (fun c -> String.make 1 c));
    Printf.printf "%s\n" (Array.get stacks 2 |> list_to_string (fun c -> String.make 1 c));
    Printf.printf "%s\n" (List.nth instructions 1 |> string_of_instruction);

    Printf.printf "\nresult: %s\n" (
      Array.fold_left (fun result stack -> result ^ (String.make 1 (top stack))) "" stacks
    )
;;

(* Calling the solutions *)

let raw_args = Args.read () in
let parsed = Args.parse raw_args in
let filename =
  match parsed.file with
  | None -> "./inputs/day5-example.txt"
  | Some name -> name
in
  if parsed.part == 1 then
    part1 filename
  else if parsed.part == 2 then
    part2 filename
;;

