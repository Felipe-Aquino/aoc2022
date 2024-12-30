open Utils

let move_number_at (arr: (int * int64) array) (at: int): unit =
  let len = Array.length arr in
  let pair = arr.(at) in
  let _, value = pair in
  if not (Int64.equal value 0L) then
  begin
    let value =
      (len - 1)
      |> Int64.of_int
      |> Int64.rem value
      |> Int64.to_int
    in
    let dest =
      if at == -value then
        len - 1
      else
      (at + value + len - 1) mod (len - 1)
    in
    let src = at in
    if dest > src then (
      for i = src to dest - 1 do
        arr.(i) <- arr.(i + 1)
      done
    ) else (
      let n = src - dest - 1 in
      for i = 0 to n do
        arr.(src - i) <- arr.(src - i - 1)
      done
    );
      arr.(dest) <- pair
  end

let mixing (numbers: (int * int64) array): unit =
  let n = Array.length numbers in
  let rec loop idx =
    if idx < n then
    begin
      let at =
        numbers
        |> Array.find_index (fun (pos, _) -> pos == idx)
        |> Option.get
      in (
        move_number_at numbers at;
        (*Array.iter (fun (_, v) -> Printf.printf "%s " (Int64.to_string v)) numbers;
        Printf.printf "\n";*)
        loop (idx + 1)
      )
    end
  in
    loop 0

let grove_coordinates_sum (numbers: (int * int64) array): int64 =
  let n = Array.length numbers in
  let zero_idx =
    numbers
    |> Array.find_index (fun (_, v) -> Int64.equal v 0L)
    |> Option.get
  in
  let (_, coord1) = numbers.((zero_idx + 1000) mod n) in
  let (_, coord2) = numbers.((zero_idx + 2000) mod n) in
  let (_, coord3) = numbers.((zero_idx + 3000) mod n) in
    Int64.add (Int64.add coord1 coord2) coord3

let part1 filename =
  let lines = read_file_lines filename in
  let numbers =
    lines
    |> List.mapi (fun i str -> (i, Int64.of_int (int_of_string str)))
    |> Array.of_list
  in
    (*Array.iter (fun (_, v) -> Printf.printf "%s " (Int64.to_string v)) numbers;
    Printf.printf "\n";*)
    mixing numbers;
    Printf.printf "coordinates' sum = %s" (Int64.to_string (grove_coordinates_sum numbers));
    Printf.printf "\n"

let mix_10_times (numbers: (int * int64) array): unit =
  let rec loop count =
    if count < 10 then
    begin
      mixing numbers;
      loop (count + 1)
    end
  in
    loop 0

let part2 filename =
  let lines = read_file_lines filename in
  let numbers =
    lines
    |> List.mapi (fun i str -> (i, Int64.mul 811589153L (Int64.of_int (int_of_string str))))
    |> Array.of_list
  in
    (*Array.iter (fun (_, v) -> Printf.printf "%s " (Int64.to_string v)) numbers;
    Printf.printf "\n";*)
    mix_10_times numbers;
    Printf.printf "coordinates' sum = %s" (Int64.to_string (grove_coordinates_sum numbers));
    Printf.printf "\n"

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day20-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

