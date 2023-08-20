open Utils

module IntPairs =
struct
  type t = int * int
  let compare (x0,y0) (x1,y1) =
    match Stdlib.compare x0 x1 with
        0 -> Stdlib.compare y0 y1
      | c -> c
end

module PairSet = Set.Make(IntPairs)

type point_t =
  { x: int
  ; y: int
  }

type rope_t =
  { knots: point_t array
  ; positions: point_t list (* tail positions *)
  }

type motion_t = 
  | Right
  | Left
  | Up
  | Down 

let motion_from_str (str: string): motion_t =
  match str with
  | "R" -> Right
  | "L" -> Left
  | "U" -> Up
  | "D" -> Down
  | _ -> failwith ("Unknow motion: " ^ str)

let point_add (p: point_t) (dx: int) (dy: int): point_t =
  { x = p.x + dx
  ; y = p.y + dy
  }

let move_head (motion: motion_t) (r: rope_t): rope_t =
  let (dx, dy) = 
    match motion with
    | Right -> 1, 0
    | Left  -> -1, 0
    | Up    -> 0, 1
    | Down  -> 0, -1
  in
    Printf.printf "head (%d, %d); %d,%d\n" (r.knots.(0).x + dx) (r.knots.(0).y + dy) dx dy;
    r.knots.(0) <- point_add (r.knots.(0)) dx dy;
    r

let test_knots_touch (r: rope_t) (start: int): bool =
  (abs (r.knots.(start - 1).x - r.knots.(start).x) < 2) &&
  (abs (r.knots.(start - 1).y - r.knots.(start).y) < 2)

let sign = function
  | c when c > 0 -> 1
  | c when c < 0 -> -1
  | _ -> 0

let update_knot (start: int) (rope: rope_t): rope_t =
  let dx = rope.knots.(start - 1).x - rope.knots.(start).x in
  let dy = rope.knots.(start - 1).y - rope.knots.(start).y in
    (* Be aware of the head being at most one step ahead *)
    if dx == 0 || dy == 0 then
      if not (test_knots_touch rope start) then
        let dx = sign dx in
        let dy = sign dy in
        let knot = point_add rope.knots.(start) dx dy in
          (* Printf.printf "tail (%d, %d)\n" knot.x knot.y; *)
          rope.knots.(start) <- knot;

          if start == (Array.length rope.knots) - 1 then
            { rope with positions = knot :: rope.positions }
          else
            rope
      else
        rope
    else begin
      if not (test_knots_touch rope start) then
        let dx = sign dx in
        let dy = sign dy in
          let knot = point_add rope.knots.(start) dx dy in
            (* Printf.printf "tail (%d, %d)\n" knot.x knot.y; *)
            rope.knots.(start) <- knot;
            if start == (Array.length rope.knots) - 1 then
              { rope with positions = knot :: rope.positions }
            else
              rope
      else
        rope
    end

let update_knots (rope: rope_t): rope_t =
  let rec loop rope' i n =
    if i < n then
      loop (update_knot i rope') (i + 1) n
    else
      rope'
  in
    loop rope 1 (Array.length rope.knots)
  
let update_rope (rope: rope_t) (motions: (motion_t * int) list): rope_t =
  let rec apply_motion_n_times rope' motion n =
    if n > 0 then
      let x =
        rope'
        |> move_head motion
        |> update_knots
      in
        apply_motion_n_times x motion (n - 1)
    else
      rope'
  in

  let rec loop rope' motions' =
    match motions' with
    | [] -> rope'
    | hd :: tl ->
      let (motion, amount) = hd in
      let x = apply_motion_n_times rope' motion amount in
        loop x tl
  in
    loop rope motions

let rec print_list = function
  | [] -> Printf.printf "\n"
  | hd :: tl ->
    Printf.printf "(%d, %d), " hd.x hd.y;
    print_list tl

let run_solution (filename: string) (knots_count: int): unit =
  let lines = read_file_lines filename in
  let motions =
    List.fold_left
      (fun result line ->
        match (String.split_on_char ' ' line) with
        | m :: amount :: _ -> (motion_from_str m, int_of_string amount) :: result
        | _ -> result
      )
      []
      lines
  in
  let rope =
    { knots = Array.init knots_count (fun i -> { x = 0; y = 0 })
    ; positions = [{ x = 0; y = 0 }]
    }
  in
  let rope = update_rope rope (List.rev motions) in
  let pairs = List.fold_left (fun acc p -> (p.x, p.y) :: acc) [] rope.positions in
  let pairs = PairSet.elements (PairSet.of_list pairs) in
    print_list rope.positions;
    Printf.printf "total: %d\n" (List.length pairs)

let part1 filename =
  run_solution filename 2

let part2 filename =
  run_solution filename 10

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day9-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

