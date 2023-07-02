open Utils

let to_digit (c: char): int =
  let code = Char.code c in
    code - 0x30

let is_digit (c: char): bool =
  let code = Char.code c in
    0x30 <= code && code <= 0x39

let read_digit (s: string) (idx: int): int option =
  let chr = String.get s idx in
    if is_digit chr then
      Some (to_digit chr)
    else
      None

let digit_list_to_num (digits: int list): int = 
  List.fold_left (fun acc value -> 10 * acc + value) 0 digits

let read_number (s: string) (idx: int): (int * int) option =
  (* iidx = internal index *)
  let rec read_digits (iidx: int): (int * int list) =
    if String.length s > iidx then
      match read_digit s iidx with
      | None -> (iidx, [])
      | Some digit ->
          match read_digits (iidx + 1) with
          | (next_idx, digits) -> (next_idx, digit :: digits)
    else
      (iidx, [])
  in
    let (next_idx, digits) = read_digits idx in
      if next_idx != idx then
        Some (next_idx, digit_list_to_num digits)
      else
        None

let read_char (s: string) (idx: int): (int * char) option =
    if String.length s > idx then
      Some (idx + 1, String.get s idx)
    else
      None

exception ReadError of string

let expect_number (s: string) (idx: int) (line: int): (int * int) =
  match read_number s idx with
  | None -> raise (
      ReadError (Printf.sprintf "%d: could not read number in '%s' at pos %d" line s idx)
    )
  | Some result -> result

let expect_char (expect: char) (s: string) (idx: int) (line: int): int =
  match read_char s idx with
  | None -> raise (
      ReadError (Printf.sprintf "%d: could not read '%c' in '%s' at pos %d" line expect s idx)
    )
  | Some (next, c) ->
    if c != expect then
      raise (
        ReadError (Printf.sprintf "%d: could not read '%c' in '%s' at pos %d" line expect s idx)
      )
    else
      next

let expect_comma = expect_char ','
let expect_dash = expect_char '-'

type range_t =
  { first: int
  ; last: int
  }

let new_range (f: int) (l: int): range_t = { first = f; last = l }
let print_range (r: range_t): unit =
  Printf.printf "range(%d..%d) " r.first r.last

(* Test if range1 fully contains range2 *)
let range_fully_contains (range1: range_t) (range2: range_t): bool =
  range1.first <= range2.first && range2.last <= range1.last

(* Test if range1 partially contains range2 *)
let range_partially_contains (range1: range_t) (range2: range_t): bool =
  not (range2.first > range1.last || range1.first > range2.last)

let parse_and_test_contained (input: string) (line: int): bool =
  let (pos, first) = expect_number input 0 line in
  let pos = expect_dash input pos line in
  let (pos, last) = expect_number input pos line in

  let range1 = new_range first last in

  let pos = expect_comma input pos line in

  let (pos, first) = expect_number input pos line in
  let pos = expect_dash input pos line in
  let (pos, last) = expect_number input pos line in

  let range2 = new_range first last in

  let ok =
    (range_fully_contains range1 range2) ||
    (range_fully_contains range2 range1)
  in
    if ok then begin
      print_range range1;
      print_range range2;
      Printf.printf "\n"
    end;
  ok

let part1 filename =
  let lines = read_file_lines filename in

  let line_idx = ref 0 in
  let test_and_sum acc line =
    line_idx := !line_idx + 1;

    if (parse_and_test_contained line !line_idx) then
      acc + 1
    else
      acc
  in
  let contained_count = List.fold_left test_and_sum 0 lines in
    Printf.printf "fully contained count: %d\n" contained_count

let parse_and_test_contained2 (input: string) (line: int): bool =
  let (pos, first) = expect_number input 0 line in
  let pos = expect_dash input pos line in
  let (pos, last) = expect_number input pos line in

  let range1 = new_range first last in

  let pos = expect_comma input pos line in

  let (pos, first) = expect_number input pos line in
  let pos = expect_dash input pos line in
  let (pos, last) = expect_number input pos line in

  let range2 = new_range first last in

  let ok = range_partially_contains range1 range2 in
    if ok then begin
      print_range range1;
      print_range range2;
      Printf.printf "\n"
    end;
    ok

let part2 filename =
  let lines = read_file_lines filename in

  let line_idx = ref 0 in
  let test_and_sum acc line =
    line_idx := !line_idx + 1;

    if (parse_and_test_contained2 line !line_idx) then
      acc + 1
    else
      acc
  in
  let contained_count = List.fold_left test_and_sum 0 lines in
    Printf.printf "partially contained count: %d\n" contained_count

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day4-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

