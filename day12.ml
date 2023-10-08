open Utils

module SI = Set.Make(Int)

(* In this problem the path length between nodes is always 1 *)

type node_t =
  { mutable visited: bool
  ; mutable distance: int
  ; mutable origin: int
  ; char_code: int
  ; neighbors: int list
  }

type dijkstra_t =
  { nodes: node_t array
  ; mutable unvisited: SI.t
  ; start_node: int
  ; end_node: int
  }

type node_ref_t =
  { idx: int
  ; distance: int
  }

let print_path ncols nodes end_idx =
  let idx_to_ij idx = idx / ncols, idx mod ncols in

  let rec loop idx result =
    let node = nodes.(idx) in
    let ij = idx_to_ij idx in
    if node.origin != -1 then
      loop node.origin (ij :: result)
    else
      ij :: result
  in
  let result = loop end_idx [] in
    List.iter (fun (i, j) -> Printf.printf "(%d, %d)\n" i j) result

let walk (dijk: dijkstra_t) (current_node: int): unit =
  let node = dijk.nodes.(current_node) in
    let neighbor_count = List.length node.neighbors in
      for i = 0 to neighbor_count - 1 do
        let neighbor_idx = List.nth node.neighbors i in
        let neighbor = dijk.nodes.(neighbor_idx) in
          if (not neighbor.visited) && neighbor.distance > node.distance + 1 then
          begin
            neighbor.distance <- node.distance + 1;
            neighbor.origin <- current_node;
          end
      done

let update_nodes_path (dijk: dijkstra_t): unit =
  let compare_distances (idx: int) (node_ref: node_ref_t): node_ref_t =
    if dijk.nodes.(idx).distance < node_ref.distance then
      { idx = idx
      ; distance = dijk.nodes.(idx).distance
      }
    else
      node_ref
  in
  let rec loop (node_idx: int) =
    walk dijk node_idx;
    dijk.unvisited <- SI.remove node_idx dijk.unvisited;

    let node = dijk.nodes.(node_idx) in
      node.visited <- true;

    let min_node =
      { idx = -1
      ; distance = Int.max_int
      }
    in
    let min_node = SI.fold compare_distances dijk.unvisited min_node in
      if min_node.idx != dijk.end_node && min_node.idx != -1 then
      begin
        loop min_node.idx
      end
      else
        Printf.printf "stopped in node %d\n" min_node.idx
  in
    loop dijk.start_node


let parse_nodes (lines: string list): int * int * (node_t array) =
  let nrows = List.length lines in
  let ncols = String.length (List.nth lines 0) in

  let get_chr_code i j =
    if i < 0 || i >= nrows || j < 0 || j >= ncols then
      10000
    else
      Char.code (String.get (List.nth lines i) j)
  in

  let directions =
    [ -1, 0
    ; +1, 0
    ; 0, -1
    ; 0, +1
    ]
  in

  let start_node = ref (-1) in
  let end_node = ref (-1) in
  let nodes =
    Array.make
      (nrows * ncols)
      { visited = false
      ; distance = Int.max_int
      ; origin = -1
      ; neighbors = []
      ; char_code = 100000
      }
  in
    for i = 0 to nrows - 1 do
      for j = 0 to ncols - 1 do
        let idx = j + i * ncols in
        let chr0 = get_chr_code i j in
        let chr =
          match chr0 with
          | c when c == Char.code 'S' ->
            start_node := idx;
            Char.code 'a'
          | c when c == Char.code 'E' ->
            end_node := idx;
            Char.code 'z'
          | _ -> chr0
        in

        let neighbors =
          List.fold_left
            (fun acc (dcol, drow) ->
              let neigh_chr0 = get_chr_code (i + drow) (j + dcol) in
              let neigh_chr =
                match neigh_chr0 with
                | c when c == Char.code 'S' -> Char.code 'a'
                | c when c == Char.code 'E' -> Char.code 'z'
                | _ -> neigh_chr0
              in
                if neigh_chr - chr <= 1 then
                  (idx + dcol + drow * ncols) :: acc
                else
                  acc
            )
            []
            directions
        in

        nodes.(idx) <-
          { nodes.(idx) with neighbors = neighbors
          ; char_code = chr
          }
      done
    done;

      (!start_node, !end_node, nodes)

let part1 filename =
  let lines = read_file_lines filename in
  let nrows = List.length lines in
  let ncols = String.length (List.nth lines 0) in

  let (start_node, end_node, nodes) = parse_nodes lines in
    nodes.(start_node) <- { nodes.(start_node) with distance = 0 };

    let dijk =
      { nodes = nodes
      ; unvisited = SI.of_list (List.init (nrows * ncols) (fun i -> i))
      ; start_node = start_node
      ; end_node = end_node
      }
    in
      Printf.printf "end: %d\n" end_node;
      update_nodes_path dijk;

      Printf.printf "distance: %d\n" nodes.(end_node).distance
      (* print_path ncols nodes !end_node *)

let reset_nodes (dijk: dijkstra_t): unit =
  Array.iter
    (fun node ->
      node.visited <- false;
      node.distance <- Int.max_int;
      node.origin <- -1
    )
    dijk.nodes

let part2 filename =
  let lines = read_file_lines filename in
  let nrows = List.length lines in
  let ncols = String.length (List.nth lines 0) in

  let (_, end_node, nodes) = parse_nodes lines in
  let possible_start_nodes =
    Array.fold_left
      (fun result (i, node) ->
        if node.char_code == Char.code 'a' then
          i :: result
        else
          result
      )
      []
      (Array.mapi (fun i node -> (i, node)) nodes)
  in
  let count = List.length possible_start_nodes in
  let distances = Array.init count (fun _ -> Int.max_int) in
    for i = 0 to count - 1 do
      let start_node = List.nth possible_start_nodes i in
      nodes.(start_node) <- { nodes.(start_node) with distance = 0 };

      let dijk =
        { nodes = nodes
        ; unvisited = SI.of_list (List.init (nrows * ncols) (fun i -> i))
        ; start_node = start_node
        ; end_node = end_node
        }
      in
        update_nodes_path dijk;

        distances.(i) <- nodes.(end_node).distance;
        Printf.printf "  %d: distance %d\n\n" start_node nodes.(end_node).distance;

        reset_nodes dijk;
    done;
      let min_distance = Array.fold_left min Int.max_int distances in
        Printf.printf "min distance: %d\n" min_distance

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day12-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

