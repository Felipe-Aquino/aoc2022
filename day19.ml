open Utils

let parse fmt input f = Scanf.bscanf (Scanf.Scanning.from_string input) fmt f

type robot_t =
  | Ore
  | Clay
  | Obsidian
  | Geode

type cost_t =
  { ore: int
  ; clay: int
  ; obsidian: int
  }

type blueprint_t =
  { id: int
  ; ore_robot_cost: cost_t
  ; clay_robot_cost: cost_t
  ; obsidian_robot_cost: cost_t
  ; geode_robot_cost: cost_t
  }

type state_t =
  { ore_robot_count: int
  ; clay_robot_count: int
  ; obsidian_robot_count: int
  ; geode_robot_count: int
  ; ore_count: int
  ; clay_count: int
  ; obsidian_count: int
  ; geode_count: int
  }

type system_t =
  { states: state_t list
  ; time_remaining: int
  ; blueprints: blueprint_t array
  ; max_num_states: int
  }

let initial_state =
  { ore_robot_count = 1
  ; clay_robot_count = 0
  ; obsidian_robot_count = 0
  ; geode_robot_count = 0
  ; ore_count = 0
  ; clay_count = 0
  ; obsidian_count = 0
  ; geode_count = 0
  }

let print_state (i: int) (s: state_t): unit =
  Printf.printf "state %d\n" i;
  Printf.printf "  ore_robot_count: %d\n" s.ore_robot_count;
  Printf.printf "  clay_robot_count: %d\n" s.clay_robot_count;
  Printf.printf "  obsidian_robot_count: %d\n" s.obsidian_robot_count;
  Printf.printf "  geode_robot_count: %d\n\n" s.geode_robot_count;
  Printf.printf "  ore_count: %d\n" s.ore_count;
  Printf.printf "  clay_count: %d\n" s.clay_count;
  Printf.printf "  obsidian_count: %d\n" s.obsidian_count;
  Printf.printf "  geode_count: %d\n\n" s.geode_count

let read_blueprint (str: string): blueprint_t =
  let f id a b c d e f =
    { id = id
    ; ore_robot_cost = { ore = a; clay = 0; obsidian = 0 }
    ; clay_robot_cost = { ore = b; clay = 0; obsidian = 0 }
    ; obsidian_robot_cost = { ore = c; clay = d; obsidian = 0 }
    ; geode_robot_cost = { ore = e; clay = 0; obsidian = f }
    }
  in
    parse
      "Blueprint %d: Each ore robot costs %d ore. Each clay robot costs %d ore. Each obsidian robot costs %d ore and %d clay. Each geode robot costs %d ore and %d obsidian."
      str
      f

let state_cmp (a: state_t) (b: state_t): int =
  if b.geode_count == a.geode_count then
    if a.geode_robot_count == b.geode_robot_count then
      if b.obsidian_count == a.obsidian_count then
        if a.obsidian_robot_count == b.obsidian_robot_count then
          if b.clay_count == a.clay_count then
            if a.clay_robot_count == b.clay_robot_count then
              if a.ore_count == b.ore_count then
                b.ore_robot_count - a.ore_robot_count
              else
                b.ore_count - a.ore_count
            else
              b.clay_robot_count - a.clay_robot_count
          else
            b.clay_count - a.clay_count
        else
          b.obsidian_robot_count - a.obsidian_robot_count
      else
        b.obsidian_count - a.obsidian_count
    else
      b.geode_robot_count - a.geode_robot_count
  else
    b.geode_count - a.geode_count

let produce_with_bot (robot: robot_t) (cost: cost_t) (state: state_t): state_t option =
  if cost.ore <= state.ore_count &&
     cost.clay <= state.clay_count &&
     cost.obsidian <= state.obsidian_count
  then
    let ore_count = state.ore_count - cost.ore + state.ore_robot_count in
    let clay_count = state.clay_count - cost.clay + state.clay_robot_count in
    let obsidian_count = state.obsidian_count - cost.obsidian + state.obsidian_robot_count in
    let geode_count = state.geode_count + state.geode_robot_count in
    let state =
      { state with ore_count = ore_count
      ; clay_count = clay_count
      ; obsidian_count = obsidian_count
      ; geode_count = geode_count
      }
    in
    let state =
      match robot with
      | Ore -> { state with ore_robot_count = state.ore_robot_count + 1 }
      | Clay -> { state with clay_robot_count = state.clay_robot_count + 1 }
      | Obsidian -> { state with obsidian_robot_count = state.obsidian_robot_count + 1 }
      | Geode -> { state with geode_robot_count = state.geode_robot_count + 1 }
    in
      Some state
  else
    None

let produce_without_bot (state: state_t): state_t =
  let ore_count = state.ore_count + state.ore_robot_count in
  let clay_count = state.clay_count + state.clay_robot_count in
  let obsidian_count = state.obsidian_count + state.obsidian_robot_count in
  let geode_count = state.geode_count + state.geode_robot_count in
  let state1 =
    { state with ore_count = ore_count
    ; clay_count = clay_count
    ; obsidian_count = obsidian_count
    ; geode_count = geode_count
    }
  in
    state1

let apply_blueprint (b: blueprint_t) (state: state_t): state_t list =
  let new_ore_bot = produce_with_bot Ore b.ore_robot_cost state in
  let new_clay_bot = produce_with_bot Clay b.clay_robot_cost state in
  let new_obsidian_bot = produce_with_bot Obsidian b.obsidian_robot_cost state in
  let new_geode_bot = produce_with_bot Geode b.geode_robot_cost state in
  let state1 = produce_without_bot state in
  let possible_states =
    [Some state1; new_ore_bot; new_clay_bot; new_obsidian_bot; new_geode_bot]
  in
    List.filter_map _id possible_states

let keep_top_n_states (n: int) (states: state_t list): state_t list =
  states 
  |> List.sort_uniq state_cmp
  |> take n

let generate_new_states (b: blueprint_t) (states0: state_t list): state_t list =
  let rec loop states result =
    match states with
    | [] -> result
    | hd :: tl ->
      let new_states = apply_blueprint b hd in
        loop tl (new_states @ result)
  in
    loop states0 []

let run (system0: system_t) (blueprint_idx: int): system_t =
  let rec loop system =
    if system.time_remaining > 0 then
    begin
      let states =
        system.states
        |> generate_new_states system.blueprints.(blueprint_idx)
        |> keep_top_n_states system0.max_num_states
      in
        loop
          { system with time_remaining = system.time_remaining - 1
          ; states = states
          }
    end
    else
      system
  in
    loop system0

let part1 filename =
  let lines = read_file_lines filename in
  let blueprints = List.map read_blueprint lines in
  let blueprints = Array.of_list blueprints in
  let blueprints_count = Array.length blueprints in
  let rec loop idx total =
    if idx < blueprints_count then
      let system =
        { time_remaining = 24
        ; blueprints = blueprints 
        ; states = [initial_state]
        ; max_num_states = 100
        }
      in
      let system = run system idx in
      let fst = List.hd system.states in
      let total = total + blueprints.(idx).id * fst.geode_count in
        (* List.iteri print_state system.states *)
        loop (idx + 1) total
    else
      total
  in
  let result = loop 0 0 in
    Printf.printf "quality level: %d\n" result

let part2 filename =
  let lines = read_file_lines filename in
  let blueprints = List.map read_blueprint lines in
  let blueprints = Array.of_list blueprints in
  let blueprints_count = Array.length blueprints in
  let blueprints_count = min 3 blueprints_count in
  let rec loop idx total =
    if idx < blueprints_count then
      let system =
        { time_remaining = 32
        ; blueprints = blueprints 
        ; states = [initial_state]
        ; max_num_states = 5000
        }
      in
      let system = run system idx in
      let fst = List.hd system.states in
      let total = total * fst.geode_count in
        loop (idx + 1) total
    else
      total
  in
  let result = loop 0 1 in
    Printf.printf "product of first 3: %d\n" result

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day19-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

