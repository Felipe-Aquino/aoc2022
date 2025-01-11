open Utils

let snafu_to_decimal (snafu: string): int =
  let rec loop idx power_of_five result =
    if idx >= 0 then
      let chr = String.get snafu idx in
      let value =
        match chr with
        | '0' -> 0
        | '1' -> 1
        | '2' -> 2
        | '-' -> -1
        | '=' -> -2
        | _ -> failwith "Unknown symbol"
      in
      let result = result + power_of_five * value in
        loop (idx - 1) (power_of_five * 5) result
    else
      result
  in
    loop ((String.length snafu) - 1) 1 0

let list_concat (item_to_string: 'a -> string) (ls: 'a list): string =
    concats (List.map item_to_string ls)

let decimal_to_snafu (decimal: int): string =
  let rec base5_digits n result =
    if n > 5 then
      let q = n / 5 in
      let r = n mod 5 in
        base5_digits q (r :: result)
    else
      n :: result
  in
  let digits = base5_digits decimal [] in
  let rec snafu_digits b5digits =
    match b5digits with
    | [] -> (false, [])
    | d :: tl ->
      let (carry, other_digits) = snafu_digits tl in
      let d = if carry then d + 1 else d in
        if d > 2 then
          (true, (d - 5) :: other_digits)
        else
          (false, d :: other_digits)
  in
  let (carry, new_digits) = snafu_digits digits in
  let digits =
    if carry then
      1 :: new_digits
    else
      new_digits
  in
    list_concat
      (function
        | 0 -> "0"
        | 1 -> "1"
        | 2 -> "2"
        | -1 -> "-"
        | -2 -> "="
        | _ -> failwith "Invalid digit"
      )
      digits

let part1 filename =
  let snafu_numbers = read_file_lines filename in
  let decimal_numbers = List.map snafu_to_decimal snafu_numbers in
  let sum = List.fold_left (fun acc n -> acc + n) 0 decimal_numbers in
  let snafu_sum = decimal_to_snafu sum in
    Printf.printf "Sum: %d\n" sum;
    Printf.printf "Snafu sum: %s\n" snafu_sum

let part2 filename = ()

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day25-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

