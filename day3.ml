open Utils

let string_to_priority_list (s: string): int list =
  let char_to_priority (l: int list) (c: char): int list =
    if Char.code c > 0x60 then
      (Char.code c - 0x60) :: l
    else
      (Char.code c - 0x41 + 27) :: l
  in
    List.rev (String.fold_left char_to_priority [] s)
;;

module SI = Set.Make(Int);;

let intersection ((l1, l2): int list * int list): int list =
  let set1 = SI.of_list l1 in
  let set2 = SI.of_list l2 in
    SI.elements (SI.inter set1 set2)
;;

let part1 filename =
  let lines = read_file_lines filename in
  let rucksacks_content = List.map string_to_priority_list lines in
  let compartiments_items: (int list * int list) list =
    List.map
      (fun sack ->
        let result = split_at sack (-1 + List.length sack / 2) in
        match result with
        | None -> ([], [])
        | Some r -> r
      )
      rucksacks_content
  in
  let shared_items = List.flatten (List.map intersection compartiments_items) in
  let sum acc value = acc + value in
  let sum_of_priorities = List.fold_left sum 0 shared_items in
    Printf.printf "sum of priorities: %d\n" sum_of_priorities
;;

let intersection_list3 (ls: int list list): int list =
  let size = List.length ls in
    if size == 1 then
      List.nth ls 0
    else if size == 2 then
      let set1 = SI.of_list (List.nth ls 0) in
      let set2 = SI.of_list (List.nth ls 1) in
        SI.elements (SI.inter set1 set2)
    else
      let set1 = SI.of_list (List.nth ls 0) in
      let set2 = SI.of_list (List.nth ls 1) in
      let set3 = SI.of_list (List.nth ls 2) in
        SI.elements (SI.inter (SI.inter set1 set2) set3)
;;

let part2 filename =
  let lines = read_file_lines filename in
  let rucksacks_content = List.map string_to_priority_list lines in
  let groups_of3: int list list list =
    split_by (fun idx value -> (idx + 1) mod 3 == 0) rucksacks_content in
  let common_items = List.map intersection_list3 groups_of3 in
    print_endline (list_to_string
      _id
      (List.map (fun el -> list_to_string string_of_int el) common_items)
    );

  let sum acc value = acc + value in
  let sum_of_priorities = List.fold_left sum 0 (List.flatten common_items) in
    Printf.printf "\nsum of priorities: %d\n" sum_of_priorities
;;

(* Calling the solutions *)

let raw_args = Args.read () in
let parsed = Args.parse raw_args in
let filename =
  match parsed.file with
  | None -> "./inputs/day3-example.txt"
  | Some name -> name
in
  if parsed.part == 1 then
    part1 filename
  else if parsed.part == 2 then
    part2 filename
;;

