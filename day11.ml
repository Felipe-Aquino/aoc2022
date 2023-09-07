open Utils

let parse fmt input f = Scanf.bscanf (Scanf.Scanning.from_string input) fmt f

type monkey_t =
  { id: int
  ; items: int list
  ; operation: int -> int
  ; divisor_test: int
  ; monkey_if_true: int
  ; monkey_if_false: int
  ; count: int
  }

let parse_items (str: string): int list =
  List.map
    (fun v -> v |> String.trim |> int_of_string)
    (String.split_on_char ',' str)

let eval_op (op: char): int -> int -> int =
  match op with
  | '+' -> ( + )
  | '-' -> ( - )
  | '*' -> ( * )
  | '/' -> ( / )
  | _ -> failwith "Unknown operation"

let parse_operation (fst: string) (op: char) (snd: string): int -> int =
  let op' = eval_op op in 
    if String.equal fst "old" then begin
      if String.equal snd "old" then
        (fun old -> (op' old old))
      else
        let v = int_of_string snd in (fun old -> (op' old v))
    end
    else begin
      let v1 = int_of_string fst in
        if String.equal snd "old" then
          (fun old -> (op' v1 old))
        else
          let v2 = int_of_string snd in (fun old -> (op' v1 v2))
    end

let read_monkey (lines: string list): monkey_t =
  match lines with
  | ln0 :: ln1 :: ln2 :: ln3 :: ln4 :: ln5 :: _ ->
    let id = parse "Monkey %i:" ln0 (fun v -> v) in
    let items = parse "  Starting items: %s@\t" ln1 parse_items in
    let op = parse "  Operation: new = %s %c %s" ln2 parse_operation in
    let div_by = parse "  Test: divisible by %i" ln3 (fun v -> v) in
    let v1 = parse "    If true: throw to monkey %i" ln4 (fun v -> v) in 
    let v2 = parse "    If false: throw to monkey %i" ln5 (fun v -> v) in 
      { id = id
      ; items = items
      ; operation = op
      ; divisor_test = div_by
      ; monkey_if_true = v1
      ; monkey_if_false = v2
      ; count = 0
      }

  | _ -> failwith "Not enough lines"

let print_monkey (m: monkey_t): unit =
  Printf.printf "%d: \n" m.id;
  Printf.printf "  items: %s\n" (List.fold_left (fun acc v -> acc ^ (string_of_int v) ^ " ") "" m.items);
  Printf.printf "  op_check: %d\n" (m.operation 4);
  Printf.printf "  v / %d ? %d : %d\n" m.divisor_test m.monkey_if_true m.monkey_if_false;
  Printf.printf "  count: %d\n" m.count

type inspection_t =
  { round: int
  ; monkeys: monkey_t array
  }

let run_a_round_of_inspection (insp: inspection_t) (f: int -> int): inspection_t =
  for i = 0 to Array.length insp.monkeys - 1 do
    let monkey = insp.monkeys.(i) in
    let items_count = List.length monkey.items in
      for j = 0 to items_count - 1 do
        let item = List.nth monkey.items j in
        let worry0 = monkey.operation item in
        let worry = f worry0  in
        let next =
          if worry mod monkey.divisor_test == 0 then
            monkey.monkey_if_true
          else
            monkey.monkey_if_false
        in
          insp.monkeys.(next) <-
            { insp.monkeys.(next) with
              items = insp.monkeys.(next).items @ [worry]
            }
      done;

      insp.monkeys.(i) <-
        { insp.monkeys.(i) with
          count = insp.monkeys.(i).count + items_count
        ; items = []
        }
  done;

  { insp with round = insp.round + 1 }

let rec run_n_rounds (insp: inspection_t) (n: int) (f: int -> int): inspection_t =
  if n > 0 then
    let insp' = run_a_round_of_inspection insp f in
      run_n_rounds insp' (n - 1) f
  else
    insp

let get_two_more_active (monkeys: monkey_t array): int * int =
  let m1 = ref (-1) in
  let m2 = ref (-1) in (* m2 is always greater or equal to m1 *)
    for i = 0 to Array.length monkeys - 1 do
      if monkeys.(i).count > !m2 then
        let aux = !m2 in
          m2 := monkeys.(i).count;
          m1 := aux
      else if monkeys.(i).count > !m1 then
        m1 := monkeys.(i).count
    done;

    !m1, !m2

let part1 filename =
  let lines = read_file_lines filename in
  let splited = split_by (fun _ v -> String.length v == 0) lines in
  let monkeys = List.map read_monkey splited in
  let insp =
    { round = 0
    ; monkeys = Array.of_list monkeys
    }
  in
  let insp = run_n_rounds insp 20 (fun worry -> worry / 3) in
    List.iter print_monkey (Array.to_list insp.monkeys);

  let (v1, v2) = get_two_more_active insp.monkeys in
    Printf.printf "\nresult: %d * %d = %d\n" v1 v2 (v1 * v2)


let part2 filename =
  let lines = read_file_lines filename in
  let splited = split_by (fun _ v -> String.length v == 0) lines in
  let monkeys = List.map read_monkey splited in
  let insp =
    { round = 0
    ; monkeys = Array.of_list monkeys
    }
  in
  let product_of_divisors = List.fold_left (fun acc m -> acc * m.divisor_test) 1 monkeys in
  (* Operating in mod 'product_of_divisors' to contain the worry increase while preserving the branching step *)
  let insp = run_n_rounds insp 10000 (fun worry -> worry mod product_of_divisors) in
    List.iter print_monkey (Array.to_list insp.monkeys);

  let (v1, v2) = get_two_more_active insp.monkeys in
    Printf.printf "\nresult: %d * %d = %d\n" v1 v2 (v1 * v2)


(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day11-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename
