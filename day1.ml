let rec group_lines (ls: string list): string list list =
  let rec loop (l: string list): (string list * string list list) =
    match l with
    | [] -> ([], [])
    | head :: tail ->
      let (curr, result) = loop tail in
        if String.length head == 0 then
          ([], curr :: result)
        else
          (head :: curr, result)
  in
    let (curr, result) = loop ls in
      if List.length curr == 0 then
        result
      else
        curr :: result
;;

open Utils
let print_calories_by_elf calories =
  let result =
    list_to_string _id (List.map (fun l -> list_to_string _id l) calories) in
      Printf.printf "%s\n\n" result
;;

let max a b = if a > b then a else b;;

let part1 filename =
  let lines = read_file_lines filename in
    let calories = group_lines lines in
      print_calories_by_elf calories;

    let sum acc value = acc + int_of_string value in
    let calories_totals = List.map (fun l -> List.fold_left sum 0 l) calories in
      Printf.printf "%s\n\n" (Utils.list_to_string string_of_int calories_totals);

    let max_total_calories = List.fold_left max 0 calories_totals in
      Printf.printf "max total calories: %d\n" max_total_calories 
;;

(* Array sorted from high to low *)
let insert_ord (arr: int array) (value: int) = 
  let last = -1 + Array.length arr in
  let i = ref 0 in
  let notQuit = ref true in
    while !notQuit do
      if arr.(!i) < value then begin
        (* moving the lower values to insert the new one *)
        for j = 0 to last - !i - 1 do
          arr.(last - j) <- arr.(last - j - 1);
        done;

        arr.(!i) <- value;

        notQuit := false
      end;

      notQuit := !notQuit && (!i < 2);
      i := !i + 1
    done
;;

let part2 filename = 
  let lines = read_file_lines filename in
  let calories = group_lines lines in
    print_calories_by_elf calories;

  let sum acc value = acc + int_of_string value in
  let calories_totals = List.map (fun l -> List.fold_left sum 0 l) calories in
  let highest_calories = [|0; 0; 0|] in
  let insert_calories = insert_ord highest_calories in
    List.iter insert_calories calories_totals;
    print_endline (Utils.list_to_string string_of_int (Array.to_list highest_calories));

  let sum_ints acc value = acc + value in
  let max_total_calories = Array.fold_left sum_ints 0 highest_calories in
      Printf.printf "max total calories: %d\n" max_total_calories 
;;

(* Calling the solutions *)

let raw_args = Utils.Args.read () in
let parsed = Utils.Args.parse raw_args in
let filename =
  match parsed.file with
  | None -> "./inputs/day1-example.txt"
  | Some name -> name
in
  if parsed.part == 1 then
    part1 filename
  else if parsed.part == 2 then
    part2 filename
;;
