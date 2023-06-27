open Utils

(* Test if a list contais a char *)
let rec includes (c: char) (ls: char list): bool =
  match ls with
  | [] -> false
  | [a] -> a == c
  | h :: t -> c == h || includes c t

(* All characters in a list are distinct? *)
let rec all_distinct (ls: char list): bool =
  (* Printf.printf "%s\n" (list_to_string (fun c -> String.make 1 c) ls); *)
  match ls with
  | [] -> false
  | [a] -> true
  | head :: tail -> not (includes head tail) && all_distinct tail


let get_marker_start (chars: char list) (char_sequence_size: int): int =
  let chars_count = List.length chars in

  let i = ref 0 in
  let quit = ref (!i > chars_count - char_sequence_size) in
    while not !quit do
      let found = all_distinct (take_from !i char_sequence_size chars) in
        if found then
          quit := true
        else begin
          i := !i + 1;
          quit := !i > chars_count - char_sequence_size
        end
    done;
    !i + char_sequence_size

let part1 filename =
  let lines = read_file_lines filename in
    List.iter (fun input -> 
      let chars = string_to_char_list input in

      let result = get_marker_start chars 4 in Printf.printf "result: %d\n" result
    ) lines

let part2 filename =
  let lines = read_file_lines filename in
    List.iter (fun input -> 
      let chars = string_to_char_list input in

      let result = get_marker_start chars 14 in Printf.printf "result: %d\n" result
    ) lines
;;

let raw_args = Args.read () in
let parsed = Args.parse raw_args in
let filename =
  match parsed.file with
  | None -> "./inputs/day6-example.txt"
  | Some name -> name
in
  if parsed.part == 1 then
    part1 filename
  else if parsed.part == 2 then
    part2 filename
;;
