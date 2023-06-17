type shape =
  | Rock
  | Paper
  | Scissors
;;

let char_to_shape (c: char): shape =
  match c with
  | 'A' | 'X' -> Rock
  | 'B' | 'Y' -> Paper
  | 'C' | 'Z' -> Scissors
  | _ -> Rock
;;

let shape_to_str (s: shape): string =
  match s with
  | Rock -> "Rock"
  | Paper -> "Paper"
  | Scissors -> "Scissors"
;;

let shape_score (s: shape): int =
  match s with
  | Rock -> 1
  | Paper -> 2
  | Scissors -> 3
;;

(* s2 is the shape I selected *)
let round_outcome (s1: shape) (s2: shape): int =
  match (s1, s2) with
  | (Paper, Rock) -> 0
  | (Rock, Paper) -> 6
  | (Scissors, Paper) -> 0
  | (Paper, Scissors) -> 6
  | (Rock, Scissors) -> 0
  | (Scissors, Rock) -> 6
  | _ -> 3
;;

let round_to_str (round: shape * shape): string =
  match round with
  | (s1, s2) -> Printf.sprintf "(%s, %s)" (shape_to_str s1) (shape_to_str s2)
;;

let calculate_score (rounds: (shape * shape) list): int =
  (* s2 is the shape I selected *)
  let calc total (s1, s2) = total + (shape_score s2) + (round_outcome s1 s2) in
    List.fold_left calc 0 rounds
;;

open Utils
let part1 filename =
  let lines = read_file_lines filename in
  let read_round line =
    (char_to_shape (String.get line 0), char_to_shape (String.get line 2))
  in
  let rounds = List.map read_round lines in
    print_endline (list_to_string round_to_str rounds);
    Printf.printf "\ntotal score: %d\n" (calculate_score rounds)
;;

let shape_from_condition (prev: shape) (cond: char): shape =
  if cond == 'X' then
    match prev with
    | Rock -> Scissors
    | Paper -> Rock
    | Scissors -> Paper
  else if cond == 'Y' then
    match prev with
    | Rock -> Rock
    | Paper -> Paper
    | Scissors -> Scissors
  else
    match prev with
    | Rock -> Paper
    | Paper -> Scissors
    | Scissors -> Rock
;;

open Utils
let part2 filename =
  let lines = read_file_lines filename in
  let read_round line =
    let s1 = char_to_shape (String.get line 0) in
    let s2 = shape_from_condition s1 (String.get line 2) in
      (s1, s2)
  in
  let rounds = List.map read_round lines in
    Printf.printf "total score: %d\n" (calculate_score rounds)
;;

(* Calling the solutions *)

let raw_args = Utils.Args.read () in
let parsed = Utils.Args.parse raw_args in
let filename =
  match parsed.file with
  | None -> "./inputs/day2-example.txt"
  | Some name -> name
in
  if parsed.part == 1 then
    part1 filename
  else if parsed.part == 2 then
    part2 filename
;;

