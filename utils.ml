let read_file_lines (file_name: string) : string list =
  let lines = ref [] in
  let ic = open_in file_name in
    try
      while true; do
        let line = input_line ic in
          lines := line :: !lines
      done;
      !lines
    with e ->
      close_in ic;
      List.rev !lines

let concats (ls: string list) : string =
  List.fold_left (fun acc item -> acc ^ item) "" ls

let split_at (ls: 'a list) (at: int) : ('a list * 'a list) option =
  let list_size = List.length ls in
    if at >= list_size || at < 0 then
      None
    else
      let first_elements = ref [] in
        let last_elements = ref [] in
          for i = 0 to at do
            first_elements := (List.nth ls i) :: !first_elements
          done;

          for i = at + 1 to list_size - 1 do
            last_elements := (List.nth ls i) :: !last_elements
          done;

          Some (!first_elements, !last_elements)

let rec take (n: int) (ls: 'a list) : 'a list =
  if n <= 0 then
    []
  else if n >= (List.length ls) then
    ls
  else
    match ls with
    | [] -> []
    | head :: tail -> head :: (take (n - 1) tail)

let rec take_while (f: 'a -> bool) (ls: 'a list) : 'a list =
  match ls with
  | [] -> []
  | head :: tail ->
    if f head then
      head :: (take_while f tail)
    else
      []

let rec take_while2 (f: 'a -> bool) (ls: 'a list) : 'a list * 'a list =
  match ls with
  | [] -> ([], [])
  | head :: tail ->
    if f head then
      match take_while2 f tail with
      | (ls', tl) -> (head :: ls', tl)
    else
      ([], ls)

let take_from (start: int) (n: int) (ls: 'a list) : 'a list =
  let list_size = List.length ls in
  let end_idx = min (start + n - 1) (list_size - 1) in
    if n < 0 || start < 0 || end_idx < 0 then
      []
    else
      let first_elements = ref [] in
        for i = start to end_idx do
          first_elements := (List.nth ls i) :: !first_elements
        done;

        List.rev !first_elements

let split_by (split_test: int -> 'a -> bool) (ls: 'a list): 'a list list =
  let result = ref [] in
  let start = ref 0 in
    for i = 0 to List.length ls - 1 do
      if split_test i (List.nth ls i) then
        let n = i - !start + 1 in
        let filtered = take_from !start n ls in
          if List.length filtered > 0 then begin
            result := filtered :: !result;
            start := i + 1;
          end
    done;

    if List.length ls > !start then begin
      let n =  List.length ls - !start in
      let filtered = take_from !start n ls in
        if List.length filtered > 0 then
          result := filtered :: !result;
    end;

    List.rev !result

let rec remove_last (ls: 'a list): 'a list =
  match ls with
  | [] -> []
  | [a] -> []
  | head :: tail -> head :: (remove_last tail)

let replace (ls: 'a list) (pos: int) (value: 'a): 'a list =
  List.mapi (fun i x -> if pos == i then value else x) ls

(* Abstract stuff begin *)
let enumerate (ls: 'a list): (int * 'a) list =
  let indexes_of ls' = List.init (List.length ls') (fun i -> i) in
    List.combine (indexes_of ls) ls

let packing (f: 'a -> 'b) (g: 'c -> 'a): ('c -> 'b) = fun v -> f (g v)

(* generates a function that accepts a tuple *)
let tuple_packing (f: 'a -> 'b): ('c * 'a -> 'b) =
  packing f (fun (k, v) -> v)

let tuple_packing_r (f: 'a -> 'b): ('a * 'c -> 'b) =
  packing f (fun (v, k) -> v)

(* Abstract stuff end *)

let string_to_char_list s = List.init (String.length s) (String.get s)

let list_to_string (item_to_string: 'a -> string) (ls: 'a list): string =
  let formater (i: int) (value: 'a) : string =
    if i + 1 == List.length ls then
      item_to_string value
    else
      (item_to_string value) ^ ", "
  in
    "[" ^ (concats (List.mapi formater ls)) ^ "]"

let _id (value: 'a): 'a = value

exception ArgumentError of string;;

let append_arg args i v =
  if i >= 0 then
    args := v :: !args

module Args = struct
  type t =
    { mutable part: int
    ; mutable file: string option
    }

  let read (): string list =
    if Array.length Sys.argv >= 2 then
      let args = ref [] in
      let f i v = append_arg args i v in
        Array.iteri f Sys.argv;
        !args
    else
      raise (ArgumentError "Missing Arguments")

  let parse (args: string list) : t =
    let parsed = { part = 0; file = None } in
      List.iter (
        fun arg -> begin
          let splitted = String.split_on_char ':' arg in
            if List.length splitted > 1 then
              let name = List.nth splitted 0 in
              let value = List.nth splitted 1 in
                if String.equal name "part" then
                  parsed.part <- int_of_string value
                else if String.equal name "file" then
                  parsed.file <- Some value
        end
      ) args;
      parsed

  let print (parsed: t) : unit =
    Printf.printf "{ part: %d, file: " parsed.part;

    match parsed.file with
    | None -> Printf.printf "(none) }\n"
    | Some name -> Printf.printf "'%s' }\n" name;

end
