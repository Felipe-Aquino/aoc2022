open Utils

type value_t =
  | VNumber of int
  | VList of value_t list

let is_digit = function
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false

let read_integer (start: int) (str: string): (value_t * int) option =
  let len = String.length str in
  let rec loop idx acc =
    if len > idx then
      let chr = String.get str idx in
        if is_digit chr then
          let total = 10 * acc + ((Char.code chr) - 0x30) in
            loop (idx + 1) total
        else if idx == start then
          None
        else
          Some (VNumber acc, idx)
    else
      None
  in
    loop start 0

let rec read_list (start: int) (str: string): (value_t * int) option =
  let len = String.length str in
  let rec loop idx acc =
    if len > idx then
      match String.get str idx with
      | '[' -> begin
        match read_list idx str with
        | None -> None
        | Some (ls, next_idx) ->
          loop next_idx (ls :: acc)
      end

      | ',' | ' ' ->
        (* Maybe test next char if needed *)
        loop (idx + 1) acc

      | ']' ->
        Some (VList (List.rev acc), idx + 1)

      | c when is_digit c -> begin
        match read_integer idx str with
        | None -> None
        | Some (num, next_idx) ->
          loop next_idx (num :: acc)
      end

      | _ -> None
    else
      None
  in
    if len > start then
      if String.get str start == '[' then
        loop (start + 1) []
      else
        None
    else
      None

let read_pairs (lines: string list): (value_t * value_t) list =
  let splitted = split_by (fun _ v -> String.length v == 0) lines in
    List.map
      (fun ls ->
        if List.length ls >= 2 then
          let (lhs, _) = 0 |> List.nth ls |> read_list 0 |> Option.get in
          let (rhs, _) = 1 |> List.nth ls |> read_list 0 |> Option.get in
            (lhs, rhs)
        else
          failwith "Fail to read value pair from input file"
      )
      splitted

let rec print_value (value: value_t): unit =
  match value with
  | VNumber n -> Printf.printf "%d" n
  | VList l ->
    Printf.printf "[";
    List.iteri
      (fun i v -> 
        print_value v;

        if i + 1 != List.length l then
          Printf.printf ","
      )
      l;
    Printf.printf "]"

type cmp_result_t =
  | InOrder
  | OutOfOrder
  | NotSureYet

let print_cmp_result = function
  | InOrder -> Printf.printf "InOrder\n"
  | OutOfOrder -> Printf.printf "OutOfOrder\n"
  | NotSureYet -> Printf.printf "NotSureYet\n"

let rec compare_values (left: value_t) (right: value_t): cmp_result_t =
  (*print_value left;
  Printf.printf " vs ";
  print_value right;
  Printf.printf "\n";
  *)

  match left with
  | VNumber n1 -> begin
    match right with
    | VNumber n2 ->
      if n1 == n2 then
        NotSureYet
      else if n2 > n1 then
        InOrder
      else
        OutOfOrder

    | VList l2 -> compare_values (VList [left]) right 
    end

  | VList l1 -> begin
    match right with
    | VNumber n2 -> compare_values left (VList [right])

    | VList l2 ->
      match l1 with
      | [] ->
        if List.length l2 == 0 then
          NotSureYet
        else
          InOrder
      | v1 :: tl1 ->
        match l2 with
        | [] -> OutOfOrder
        | v2 :: tl2 ->
          match compare_values v1 v2 with
          | NotSureYet ->
            compare_values (VList tl1) (VList tl2)
          | result -> result
    end


let part1 filename =
  let lines = read_file_lines filename in
  let pairs = read_pairs lines in
  let total =
    List.fold_left
      (fun acc (i, (v1, v2)) ->
        Printf.printf "(%d)\n" (i + 1);
        let result = compare_values v1 v2 in
          (*print_value v1;
          Printf.printf "\n";
          print_value v2;
          Printf.printf "\n";*)
          print_cmp_result result;
          Printf.printf "\n";

          match result with
          | InOrder -> acc + i + 1 
          | _ -> acc
      )
      0
      (enumerate pairs)
  in
    Printf.printf "total: %d\n" total


let read_values_from_lines (lines: string list): value_t list =
  List.filter_map
    (fun line ->
      if String.length line == 0 then
        None
      else
        match read_list 0 line with
        | None -> None
        | Some (value, _) -> Some value
    )
    lines

let find_value_idx (v: value_t) (values: value_t list): int option =
  List.find_map
    (fun (i, other) ->
      match compare_values v other with
      | NotSureYet -> Some (i + 1)
      | _ -> None
    )
    (enumerate values)

let part2 filename =
  let lines = read_file_lines filename in
  let lines = "[[2]]" :: ("[[6]]" :: lines) in
  let values = read_values_from_lines lines in
  let sorted =
    List.sort
      (fun v1 v2 ->
        match compare_values v1 v2 with
        | InOrder -> 0
        | _ -> 1
      )
      values
  in
    List.iteri
      (fun i value ->
        Printf.printf "(%d) " (i + 1);
        print_value value;
        Printf.printf "\n"
      )
      sorted;

  let idx1 = sorted |> find_value_idx (VList [VList [VNumber 2]]) |> Option.get in
  let idx2 = sorted |> find_value_idx (VList [VList [VNumber 6]]) |> Option.get in
    Printf.printf "key: %d * %d = %d\n" idx1 idx2 (idx1 * idx2)

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day13-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

