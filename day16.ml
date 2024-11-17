open Utils

module SI = Set.Make(Int)

(* In this problem the path length between nodes is always 1 *)

type node_t =
  { mutable visited: bool
  ; mutable distance: int
  ; mutable origin: int
  ; neighbors: int array
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

let walk (dijk: dijkstra_t) (current_node: int): unit =
  let node = dijk.nodes.(current_node) in
    let neighbor_count = Array.length node.neighbors in
      for i = 0 to neighbor_count - 1 do
        let neighbor_idx = node.neighbors.(i) in
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
      ; distance = 1000000
      }
    in
    let min_node = SI.fold compare_distances dijk.unvisited min_node in
      if min_node.idx != dijk.end_node && min_node.idx != -1 then
      begin
        loop min_node.idx
      end
      (* else Printf.printf "stopped in node %d\n" min_node.idx *)
  in
    loop dijk.start_node


let parse fmt input f = Scanf.bscanf (Scanf.Scanning.from_string input) fmt f

type valve_t =
  { name: string
  ; flow_rate: int
  ; tunnels: string list
  ; tunnels_idxs: int array
  ; mutable is_open: bool
  }

type state_t =
  { valves: valve_t array
  ; time_remaining: int
  ; total_flow: int
  ; first_valve: int
  ; distances: int array array
  }

let build_distances_matrix (valves: valve_t array): int array array =
  let make_unvisited_set () = SI.of_list (Array.to_list (Array.mapi (fun i _ -> i) valves)) in
  let valves_count = Array.length valves in
  let distances =
    Array.init
      valves_count
      (fun i -> Array.init valves_count (fun j -> if i == j then 0 else 1000000))
  in
  for i = 0 to valves_count - 1 do
    for j = i to valves_count - 1 do
      if i != j then
      begin
        let nodes = Array.map
          (fun (valve: valve_t): node_t ->
            { visited = false
            ; distance = 1000000
            ; origin = -1
            ; neighbors = valve.tunnels_idxs
            }
          )
          valves
        in
        nodes.(i).distance <- 0;
        let dijk =
          { nodes = nodes
          ; unvisited = make_unvisited_set ()
          ; start_node = i
          ; end_node = j
          }
        in
          update_nodes_path dijk;
        
        let ij_distance = nodes.(j).distance in
          distances.(i).(j) <- ij_distance;
          distances.(j).(i) <- ij_distance;
      end
    done;
  done;

  distances

let state_print_current_valve (state: state_t) =
  Printf.printf
    "(%s, %d, %d)\n"
    state.valves.(state.first_valve).name
    state.time_remaining
    state.total_flow


let print_flows flows =
  List.iter
    (fun (flow, dist, j) -> Printf.printf " (%d, %d, %d)" flow dist j)
    flows;
  Printf.printf "\n"


type path_t =
  { remaining: int
  ; total_flow: int
  ; visited: bool array
  ; last_idx: int
  ; mutable is_done: bool
  }

let path_init (valves: valve_t array): path_t =
  { remaining = 30
  ; total_flow = 0
  ; visited = Array.init (Array.length valves) (fun i -> valves.(i).flow_rate == 0)
  ; last_idx = -1
  ; is_done = false
  }

let path_copy_and_add (path: path_t) (state: state_t) (idx: int): path_t option =
  let start_idx = path.last_idx in
  let dist = state.distances.(start_idx).(idx) in
  let flow = state.valves.(idx).flow_rate * (path.remaining - dist - 1) in

  if flow <= 0 then
    None
  else
  begin
    let new_path =
      { visited = Array.copy path.visited
      ; last_idx = idx
      ; remaining = path.remaining - dist - 1
      ; total_flow = path.total_flow + flow
      ; is_done = false
      }
    in
      new_path.visited.(idx) <- true;
      new_path.is_done <- not (Array.exists (fun v -> not v) new_path.visited);
      Some new_path
  end

let calculate_flow (state0: state_t): path_t list =
  let n = Array.length state0.valves in
  let rec step (paths: path_t list) = 
    match paths with
    | [] -> []
    | path :: paths_tail ->
      if path.is_done then
        path :: (step paths_tail)
      else
      begin
        let new_paths = ref [] in
        for i = 0 to n - 1 do
          if not path.visited.(i) then
            let new_path_opt = path_copy_and_add path state0 i in
              if Option.is_some new_path_opt then
                new_paths := (Option.get new_path_opt) :: !new_paths
        done;

        if not (List.is_empty !new_paths) then
          !new_paths @ (step paths_tail)
        else
        begin
          path.is_done <- true;
          path :: (step paths_tail)
        end
      end
  in
  let rec max_path_flow paths =
    List.fold_left
      (fun mx path ->
        if path.total_flow > mx then
          path.total_flow
        else
          mx
      )
      0
      paths
  in
  let rec prune_shortests_paths (threshold: int) (paths: path_t list): path_t list =
    match paths with
    | [] -> []
    | path :: paths_tail ->
      if threshold > path.total_flow then
        prune_shortests_paths threshold paths_tail
      else
        path :: (prune_shortests_paths threshold paths_tail)
  in
  let rec loop paths iters =
    let mx = max_path_flow paths in
    let paths = paths |> step |> prune_shortests_paths mx in
    let all_done = not (List.exists (fun p -> not p.is_done) paths) in

    if all_done || iters <= 0 then
    begin
      Printf.printf "path size: %d\n" (List.length paths);
      Printf.printf "iters: %d\n" iters;
      Printf.printf "mx: %d\n" mx;

      paths
    end
    else
      loop paths (iters - 1)
  in
    let start_path = path_init state0.valves in
    let start_path = { start_path with last_idx = state0.first_valve } in
      loop [start_path] 1000


let parse_valve (line: string): valve_t =
  let f name rate a b c tunnels_str =
    { name = name
    ; flow_rate = rate
    ; tunnels =
      tunnels_str
        |> String.trim
        |> string_split_on_string ", " 
    ; tunnels_idxs = Array.make 1 0
    ; is_open = false
    }
  in
    parse "Valve %s has flow rate=%d; %s %s to %s %[ ,A-Z]" line f

let part1 filename =
  let lines = read_file_lines filename in
  let valves = List.fold_left (fun result line -> (parse_valve line) :: result) [] lines in
  let valves = List.sort (fun a b -> b.flow_rate - a.flow_rate) valves in
  let enum_valves = enumerate valves in
  let valves = List.map
    (fun valve ->
      let idxs = 
        valve.tunnels
          |> List.map
            (fun vname -> List.find (fun (_, v) -> String.equal v.name vname) enum_valves)
          |> List.map (fun (i, _) -> i)
      in
        { valve with tunnels_idxs = Array.of_list idxs }
    )
    valves
  in
  let valves = Array.of_list valves in
  let first_valve_opt = Array.find_index (fun v -> String.equal v.name "AA") valves in
  let mtx = build_distances_matrix valves in
  let valves_count = Array.length valves in
    Array.iter
      (fun v ->
        Printf.printf
          "(%s, %d, %s, %s)\n"
          v.name
          v.flow_rate
          (list_to_string _id v.tunnels)
          (list_to_string string_of_int (Array.to_list v.tunnels_idxs))
      )
      valves;

    for i = 0 to valves_count - 1 do
      for j = 0 to valves_count - 1 do
        Printf.printf "%d " mtx.(i).(j);
      done;
      Printf.printf "\n"
    done;

    let state: state_t =
      { valves = valves
      ; time_remaining = 30
      ; total_flow = 0
      ; first_valve = Option.get first_valve_opt
      ; distances = mtx
      }
    in
    let paths = calculate_flow state in
    let max_flow = List.fold_left (fun r p -> max r p.total_flow) 0 paths in
      Printf.printf "max flow: %d\n" max_flow
      (*
      List.iter
        (fun p ->
          Printf.printf
            "%d, %d, %b| %d\n"
            p.total_flow
            p.remaining
            p.is_done
            (path_remaining_flow p valves)
        )
        paths
      *)

type course_t =
  { remaining: int
  ; last_idx: int
  ; total_flow: int
  ; is_done: bool
  ; iter: int
  }

type path2_t =
  { visited: bool array
  ; course1: course_t
  ; course2: course_t
  ; done_count: int
  ; mutable is_done: bool
  }

let path2_init (idx: int) (valves: valve_t array): path2_t =
  let c1 =
    { remaining = 26
    ; last_idx = idx
    ; total_flow = 0
    ; is_done = false
    ; iter = 0
    }
  in
  let c2 =
    { remaining = 26
    ; last_idx = idx
    ; total_flow = 0
    ; is_done = false
    ; iter = 0
    }
  in
  let done_count =
    Array.fold_left
      (fun acc v -> if v.flow_rate == 0 then acc + 1 else acc)
      0
      valves
  in
    { visited = Array.map (fun v -> v.flow_rate == 0) valves
    ; course1 = c1
    ; course2 = c2
    ; done_count = done_count
    ; is_done = false
    }

let course_copy_and_add (course: course_t) (state: state_t) (idx: int): course_t option =
  if course.is_done then
    None
  else
  begin
    let start_idx = course.last_idx in
    let dist = state.distances.(start_idx).(idx) in
    let flow = state.valves.(idx).flow_rate * (course.remaining - dist - 1) in

    if flow <= 0 then
      None
    else
      Some
        { last_idx = idx
        ; remaining = course.remaining - dist - 1
        ; total_flow = course.total_flow + flow
        ; is_done = false
        ; iter = course.iter + 1
        }
  end

let calculate_flow2 (state: state_t): path2_t list =
  let n = Array.length state.valves in
  let rec step paths = 
    match paths with
    | [] -> []
    | path :: paths_tail ->
      if path.is_done then
        path :: (step paths_tail)
      else
      begin
        let found = ref false in
        let new_paths = ref [] in
        for i = 0 to n - 1 do
          if not path.visited.(i) then
          begin
            found := false;

            let course_opt1 = course_copy_and_add path.course1 state i in
              for j = 0 to n - 1 do
                if i != j && (not path.visited.(j)) then
                begin
                  found := true;

                  let course_opt2 = course_copy_and_add path.course2 state j in
                    match course_opt1, course_opt2 with
                    | Some c1, Some c2 ->
                      let new_path =
                        { visited = Array.copy path.visited
                        ; course1 = c1
                        ; course2 = c2
                        ; done_count = path.done_count + 2
                        ; is_done = (path.done_count + 2 == n)
                        }
                      in
                        new_path.visited.(i) <- true;
                        new_path.visited.(j) <- true;
                        new_paths := new_path :: !new_paths
                    | Some c1, None ->
                      let new_path =
                        { visited = Array.copy path.visited
                        ; course1 = c1
                        ; course2 = { path.course2 with is_done = true }
                        ; done_count = path.done_count + 1
                        ; is_done = (path.done_count + 1 == n)
                        }
                      in
                        new_path.visited.(i) <- true;
                        new_paths := new_path :: !new_paths
                    | None, Some c2 ->
                      let new_path =
                        { visited = Array.copy path.visited
                        ; course1 = { path.course1 with is_done = true }
                        ; course2 = c2
                        ; done_count = path.done_count + 1
                        ; is_done = (path.done_count + 1 == n)
                        }
                      in
                        new_path.visited.(j) <- true;
                        new_paths := new_path :: !new_paths
                    | None, None -> ()

                end
              done;

              if (not !found) && Option.is_some course_opt1 then
                let course1 = (Option.get course_opt1) in
                  if course1.iter > 1 then
                  begin
                    let new_path =
                      { visited = Array.copy path.visited
                      ; course1 = course1
                      ; course2 = { path.course2 with is_done = true }
                      ; done_count = path.done_count + 1
                      ; is_done = (path.done_count + 1 == n)
                      }
                    in
                      new_path.visited.(i) <- true;
                      new_paths := new_path :: !new_paths
                  end
          end
        done;

        if not (List.is_empty !new_paths) then
          !new_paths @ (step paths_tail)
        else
        begin
          path.is_done <- true;
          path :: (step paths_tail)
        end
      end
  in
  let rec max_path_flow paths =
    List.fold_left
      (fun mx path ->
        let total_flow = path.course1.total_flow + path.course2.total_flow in

        if total_flow > mx then
          total_flow
        else
          mx
      )
      0
      paths
  in
  let rec prune_shortests_paths (threshold: int) (paths: path2_t list): path2_t list =
    match paths with
    | [] -> []
    | path :: paths_tail ->
      if threshold > path.course1.total_flow + path.course2.total_flow then
        (prune_shortests_paths threshold paths_tail)
      else
        path :: (prune_shortests_paths threshold paths_tail)
  in
  let rec loop paths iters =
    let mx = max_path_flow paths in
    let paths = paths |> step |> prune_shortests_paths mx in
    let all_done = not (List.exists (fun p -> not p.is_done) paths) in

    if all_done || iters <= 0 then
    begin
      Printf.printf "path size: %d\n" (List.length paths);
      Printf.printf "iters: %d\n" iters;
      Printf.printf "mx: %d\n" mx;

      paths
    end
    else
      loop paths (iters - 1)
  in
    let start_path = path2_init state.first_valve state.valves in
      loop [start_path] 10

let part2 filename =
  let lines = read_file_lines filename in
  let valves = List.fold_left (fun result line -> (parse_valve line) :: result) [] lines in
  let valves = List.sort (fun a b -> b.flow_rate - a.flow_rate) valves in
  let enum_valves = enumerate valves in
  let valves = List.map
    (fun valve ->
      let idxs = 
        valve.tunnels
          |> List.map
            (fun vname -> List.find (fun (_, v) -> String.equal v.name vname) enum_valves)
          |> List.map (fun (i, _) -> i)
      in
        { valve with tunnels_idxs = Array.of_list idxs }
    )
    valves
  in
  let valves = Array.of_list valves in
  let first_valve_opt = Array.find_index (fun v -> String.equal v.name "AA") valves in
  let mtx = build_distances_matrix valves in
  let state: state_t =
    { valves = valves
    ; time_remaining = 30
    ; total_flow = 0
    ; first_valve = Option.get first_valve_opt
    ; distances = mtx
    }
  in
  let paths = calculate_flow2 state in
  let max_flow =
    List.fold_left
    (fun r p -> max r (p.course1.total_flow + p.course2.total_flow))
    0
    paths
  in
    (*
    List.iter
      (fun p ->
        Printf.printf
          "c1: (%d, %b, %d, %d, %d), c2: (%d, %b, %d, %d, %d), %b, %d\n"
          p.course1.iter
          p.course1.is_done
          p.course1.total_flow
          p.course1.remaining
          p.course1.last_idx
          p.course2.iter
          p.course2.is_done
          p.course2.total_flow
          p.course2.remaining
          p.course2.last_idx
          p.is_done
          p.done_count
      )
      paths;
      *)
    Printf.printf "max flow: %d\n" max_flow

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day16-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename
