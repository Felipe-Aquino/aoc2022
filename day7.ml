open Utils

(* stack operations *)
let push (value: 'a) (stack: 'a list): 'a list = value :: stack

let pop (stack: 'a list): 'a list =
  match stack with
  | [] -> []
  | head :: tail -> tail

let top (stack: 'a list): 'a =
  match stack with
  | [] -> failwith "Empty stack"
  | head :: tail -> head

(* entries *)
type entry =
  | Ls
  | Cd of string
  | FileEntry of string * int
  | DirEntry of string

let read_entry (input: string): entry =
  let items = String.split_on_char ' ' input in
  let first = List.nth items 0 in
  let second = List.nth items 1 in

  if String.equal first "$" then begin
    if String.equal second "ls" then
      Ls
    else if String.equal second "cd" then
      Cd (List.nth items 2)
    else
      failwith (Printf.sprintf "Unknown command %s" second)
  end
  else if String.equal first "dir" then
    DirEntry second
  else
    FileEntry (second, int_of_string first)

let match_file_or_dir = function
  | Ls -> false
  | Cd _ -> false
  | _ -> true

type filetree =
  | File of { name: string; size: int }
  | Dir of
    { name: string
    ; mutable content: filetree list
    ; mutable size: int
    }

let spaces n =
  let rec aux depth result =
    if depth <= 0 then result
    else aux (depth - 1) ("  " ^ result)
  in
    aux n ""

let filetree_print (ft: filetree) : unit =
  let rec loop ft' depth =
    match ft' with
    | File f -> Printf.printf "%s- %s (file, size=%d)\n" (spaces depth) f.name f.size
    | Dir d ->
      Printf.printf "%s- %s (dir, size=%d)\n" (spaces depth) d.name d.size;
      if List.length d.content > 0 then
        for i = 0 to List.length d.content - 1 do
          loop (List.nth d.content i) (depth + 1)
        done
  in
    loop ft 0

let rec entry_list_as_tree_list (entries: entry list): filetree list =
  match entries with
  | [] -> []
  | hd :: tl ->
    let item =
      match hd with
      | FileEntry (name, size) -> File { name = name; size = size }
      | DirEntry (name) -> Dir { name = name; content = []; size = 0 }
      | _ -> failwith "Expecting file or dir"
    in
      item :: entry_list_as_tree_list tl

(* A ls command generates a filetree list and the remaing *)
let ls_command (entries: entry list) : filetree list * entry list =
  let (consumed, remaining) = take_while2 match_file_or_dir entries in
  let tree_list = entry_list_as_tree_list consumed in
    tree_list, remaining

let is_dir_with_name (name: string) (node: filetree) : bool =
  match node with
  | Dir d -> String.equal d.name name
  | _ -> false

(* Try to set the contents of a dir *)
let update_dir (dir: filetree) (content: filetree list): unit =
  match dir with
  | Dir d -> d.content <- content
  | _ -> failwith "Expecting dir"


type parse_state =
  { mutable entries: entry list
  ; mutable stack: filetree list
  ; mutable curr: filetree
  ; root: filetree
  }

(* Read the comands and build the tree *)
let rec process_commands (entries: entry list) : filetree =
  let rec loop (state: parse_state) : unit =
    match state.entries with
    | [] -> ()
    | hd :: tl ->
      (match hd with
      | Ls ->
        let (ft_list, remaining) = ls_command tl in
        state.entries <- remaining;
        update_dir state.curr ft_list;

      | Cd name ->
        if String.equal name "/" then begin
          if is_dir_with_name "/" state.curr then
            state.entries <- tl
          else
            state.curr <- top state.stack;
            state.stack <- pop state.stack
        end
        else if String.equal name ".." then begin
          state.entries <- tl;
          state.curr <- top state.stack;
          state.stack <- pop state.stack
        end
        else
          let next_dir =
            match state.curr with
            | Dir d -> List.find (is_dir_with_name name) d.content
            | _ -> failwith "Not a directory"
          in
            state.entries <- tl;
            state.stack <- push state.curr state.stack;
            state.curr <- next_dir
      | _ -> failwith "Expecting command");

      (loop [@tailcall]) state
  in
    let root = Dir { name = "/"; content = []; size = 0 } in
    let state =
      { entries = entries
      ; root = root 
      ; curr = root 
      ; stack = []
      }
    in
      loop state;
      state.root

let filetree_item_size = function
  | File f -> f.size
  | Dir d -> d.size

let rec fill_directories_sizes (ft: filetree): unit =
  match ft with
  | Dir d ->
    let size = List.length d.content in
      if size > 0 then
        for i = 0 to size - 1 do
          let item = List.nth d.content i in
            fill_directories_sizes item;
            d.size <- d.size + filetree_item_size item
        done
  | _ -> ()

let rec sum_dirs_less_than_100k (ft: filetree): int =
  match ft with
  | Dir d ->
    let value = if d.size <= 100000 then d.size else 0 in
    let total =
      List.fold_left (fun acc item -> acc + sum_dirs_less_than_100k item) 0 d.content
    in
      value + total
  | _ -> 0

let part1 filename =
  let lines = read_file_lines filename in
  let entries = List.map read_entry lines in
  let tree = process_commands entries in
    fill_directories_sizes tree;
    filetree_print tree;
    Printf.printf "result: %d\n" (sum_dirs_less_than_100k tree)


let rec smallest_size_to_delete (ft: filetree) (target_size: int) (min_found: int): int =
  match ft with
  | Dir d ->
    let value =
      if d.size >= target_size then
        min d.size min_found
      else
        min_found
    in
      List.fold_left
        (fun current item -> smallest_size_to_delete item target_size current)
        value
        d.content
  | _ -> min_found

let part2 filename =
  let lines = read_file_lines filename in
  let entries = List.map read_entry lines in
  let tree = process_commands entries in
    fill_directories_sizes tree;
    filetree_print tree;

  let root_size = filetree_item_size tree in
  let target_size = root_size - 40000000 in
    Printf.printf "result: %d\n" (smallest_size_to_delete tree target_size root_size)

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day7-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

