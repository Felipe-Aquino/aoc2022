open Utils

type
  node_value_t =
    | Undefined
    | Number of int
    | BinOp of
      { lhs: node_t
      ; rhs: node_t
      ; op: int -> int -> int
      ; op_name: char
      }
  and
    node_t =
    { name: string
    ; mutable value: node_value_t
    }

let times a b = a * b
let safe_div a b = if b != 0 then a / b else (Int.max_int / 10)

let is_alphanum (c: char): bool =
  (c >= 'a' && c <= 'z') ||
  (c >= 'A' && c <= 'Z') ||
  (c >= '0' && c <= '9')

let parse_line (line: string): string list =
  match String.split_on_char ' ' line with
  | hd :: tl -> (String.sub hd 0 ((String.length hd) - 1)) :: tl
  | [] -> []

let take2 ls =
  match ls with
  | a :: b :: tl -> (a, b)
  | _ -> failwith "take2 unreachable"

let take4 ls =
  match ls with
  | a :: b :: c :: d :: tl -> (a, b, c, d)
  | _ -> failwith "take4 unreachable"

let read_number_node (tokens: string list) (symbols: (string, node_t) Hashtbl.t): unit =
  let (name, value) = take2 tokens in
    match Hashtbl.find_opt symbols name with
    | None ->
      let n = { name = name; value = Number (int_of_string value) } in
        Hashtbl.add symbols name n
    | Some n ->
      n.value <- Number (int_of_string value)

let read_binop_node (tokens: string list) (symbols: (string, node_t) Hashtbl.t): unit =
  let (name, lhs_name, op_name, rhs_name) = take4 tokens in
  let lhs =
    match Hashtbl.find_opt symbols lhs_name with
    | None ->
      let n = { name = lhs_name; value = Undefined } in
        Hashtbl.add symbols lhs_name n;
        n
    | Some n -> n 
  in
  let rhs =
    match Hashtbl.find_opt symbols rhs_name with
    | None ->
      let n = { name = rhs_name; value = Undefined } in
        Hashtbl.add symbols rhs_name n;
        n
    | Some n -> n 
  in
  let op =
    if String.equal op_name "+" then
      (+)
    else if String.equal op_name "-" then
      (-)
    else if String.equal op_name "*" then
      times
    else if String.equal op_name "/" then
      safe_div (*/*)
    else
      failwith ("Unknown operator = " ^ op_name)
  in
  let binop = 
    BinOp
    { lhs = lhs
    ; rhs = rhs
    ; op = op
    ; op_name = (String.get op_name 0)
    }
  in
    match Hashtbl.find_opt symbols name with
    | None ->
      let nd =
        { name = name
        ; value = binop
        }
      in
        Hashtbl.add symbols name nd
    | Some n -> n.value <- binop

let parse (lines: string list): node_t * node_t =
  let symbols: (string, node_t) Hashtbl.t = Hashtbl.create (List.length lines) in
  let rec loop lines0 =
    match lines0 with
    | line :: remaining ->
      begin 
      let tokens = parse_line line in
        begin
        match List.length tokens with
        | 2 -> read_number_node tokens symbols
        | 4 -> read_binop_node tokens symbols
        | _ -> failwith "Wrong number of tokens"
        end;

        if not (List.is_empty remaining) then
          loop remaining
      end
    | [] -> ()
  in
    loop lines;

    let root = Hashtbl.find symbols "root" in
    let humn = Hashtbl.find symbols "humn" in
      (root, humn)

let print_node (node: node_t): unit =
  let rec loop (n: node_t) (depth: int): unit =
    let pad = String.concat "  " (List.init depth (fun _ -> "")) in
      Printf.printf "%s %d. %s: " pad depth n.name;
      match n.value with
      | Undefined -> Printf.printf "undefined\n"
      | Number v -> Printf.printf "number = %d\n" v
      | BinOp { lhs; rhs; op_name } -> (
        Printf.printf "binop = %c\n" op_name;
        loop lhs (depth + 1);
        loop rhs (depth + 1)
      )
  in
    loop node 1

let calculate (node: node_t): int =
  let rec loop (n: node_t): int =
      match n.value with
      | Undefined -> failwith "Should never be unknown"
      | Number v -> v
      | BinOp { lhs; rhs; op } -> (
        let v1 = loop lhs in
        let v2 = loop rhs in
          op v1 v2
      )
  in
    loop node

let part1 filename =
  let lines = read_file_lines filename in
  let (root_node, _) = parse lines in
    (* print_node root_node *)
    let result = calculate root_node in
      Printf.printf "Number yelled by root monkey: %d\n" result

let get_node_number (node: node_t): int =
  match node.value with
  | Number v -> v
  | _ -> failwith "fail to get node number"

let sign v =
  if v > 0 then
    1
  else if v < 0 then
    -1
  else
    0

let binary_search_humn (root: node_t) (humn: node_t): unit =
  humn.value <- Number 0;

  let first_humn = ref 0 in
  let first_sign = sign (calculate root) in

  let second_humn = ref 310 in
  let second_sign = ref first_sign in
    humn.value <- Number (!second_humn);
    (* Printf.printf "first: %d\n" first_sign; *)

  let rec loop1 iters =
    if iters > 0 then
    begin
      let calc = calculate root in
      second_sign := sign (calc);

      (* Printf.printf "second: %d, %d, %d\n" !second_humn !second_sign calc; *)

      if !second_sign == first_sign then (
        first_humn := !second_humn;
        second_humn := !second_humn * 2;
        humn.value <- Number (!second_humn);
        loop1 (iters - 1)
      )
    end
  in
    loop1 100;

  let rec finding (a: int) (b: int) (iters: int): int =
    let p = (a + b) / 2 in
      humn.value <- Number p;
      Printf.printf "a = %d, b = %d, p = %d\n" a b p;
    if iters > 0 then
    begin
      let s = sign (calculate root) in
        if s == first_sign then
          finding p b (iters - 1)
        else if s == !second_sign then
          finding a p (iters - 1)
        else
          p
    end
    else p
  in
  let found = finding !first_humn !second_humn 100 in
    humn.value <- Number (found - 2);
  let case0 = sign (calculate root) in
    humn.value <- Number (found - 1);
  let case1 = sign (calculate root) in
    humn.value <- Number (found);
  let case2 = sign (calculate root) in
    humn.value <- Number (found + 1);
  let case3 = sign (calculate root) in
    Printf.printf "\nfound = %d, ok = %b\n" (found - 2) (case0 == 0);
    Printf.printf "found = %d, ok = %b\n" (found - 1) (case1 == 0);
    Printf.printf "found = %d, ok = %b\n" (found + 0) (case2 == 0);
    Printf.printf "found = %d, ok = %b\n" (found + 1) (case3 == 0)

let part2 filename =
  let lines = read_file_lines filename in
  let (root_node, humn_node) = parse lines in
  let new_root_value =
    match root_node.value with
    | BinOp binop -> BinOp { binop with op = (-); op_name = '-' }
    | _ -> root_node.value
  in
    root_node.value <- new_root_value;
    (* print_node root_node; *)
    binary_search_humn root_node humn_node

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day21-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

