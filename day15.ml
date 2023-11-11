open Utils

let parse fmt input f = Scanf.bscanf (Scanf.Scanning.from_string input) fmt f

type point_t =
  { x: int
  ; y: int
  }

type sensor_t =
  { pos: point_t
  ; beacon_pos: point_t
  }

type state_t =
  { sensors: sensor_t list
  ; beacons: point_t list
  }

let read_points sensor_x sensor_y beacon_x beacon_y =
  { x = sensor_x; y = sensor_y }, { x = beacon_x; y = beacon_y } 

let manhattan_distance p1 p2 =
  abs (p2.x - p1.x) + abs (p2.y - p1.y)

let join_ranges ((a1, b1): int * int) ((a2, b2): int * int): (int * int) option =
  if b1 < a2 || a1 > b2 then
  begin
    if b1 < a2 && a2 - b1 == 1 then
      Some (a1, b2)
    else if a1 > b2 && a1 - b2 == 1 then
      Some (a2, b1)
    else
      None
  end
  else
    Some (min a1 a2, max b1 b2)

let reduce_ranges (ranges: (int * int) array): (int * int) array =
  let size = ref (Array.length ranges) in
  let i = ref 0 in
  let j = ref (!size - 1) in
    while !i < !size do
      while !j > !i do
        let current = ranges.(!i) in
        let other = ranges.(!j) in
          match join_ranges current other with
          | Some new_current -> (
            ranges.(!j) <- ranges.(!size - 1); (* remove swap *)
            ranges.(!i) <- new_current;
            decr size;
            j := !size - 1
          )
          | None -> (
            decr j
          )
      done;

      j := !size - 1;
      incr i
    done;

    Array.sub ranges 0 !size

let part1 filename =
  let lines = read_file_lines filename in
  let pairs =
    List.map (fun line ->
      parse "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d" line read_points
    ) lines
  in
  let state =
    { sensors = []
    ; beacons = []
    }
  in
  let state = List.fold_left
    (fun result (sensor_pt, beacon_pt)-> 
      let existing = List.find_opt
        (fun b -> beacon_pt.x == b.x && beacon_pt.y == b.y)
        result.beacons
      in
        match existing with
        | None -> 
          let sensor =
            { pos = sensor_pt
            ; beacon_pos = beacon_pt
            }
          in
            { sensors = sensor :: result.sensors
            ; beacons = beacon_pt :: result.beacons
            }
        | Some b ->
          let sensor =
            { pos = sensor_pt
            ; beacon_pos = b
            }
          in
            { result with sensors = sensor :: result.sensors }
      )
      state
      pairs
  in
  let state =
    { sensors = List.rev state.sensors
    ; beacons = List.rev state.beacons
    }
  in
    (*
    List.iter
      (fun sensor ->
        Printf.printf "(%d, %d) -> (%d, %d) : dist = %d\n" sensor.pos.x sensor.pos.y sensor.beacon_pos.x sensor.beacon_pos.y (manhattan_distance sensor.pos sensor.beacon_pos)
      )
      state.sensors;

    List.iter
      (fun beacon ->
        Printf.printf "(%d, %d)\n" beacon.x beacon.y
      )
      state.beacons;
    *)

    let y0 =
      if String.equal filename "./inputs/day15-example.txt" then
        10
      else
        2000000
    in
    let beacons_in_y0_count =
      List.fold_left
        (fun t beacon -> if beacon.y == y0 then t + 1 else t)
        0
        state.beacons
    in
      Printf.printf "# beacons over the line y=%d: %d\n\n" y0 beacons_in_y0_count;

    (* |x - x0| + |y - 10| <= dist *)
    (* x0 - dist + abs (y0 - 10) <= x <= x0 + dist - abs (y0 - 10) *)
    let ranges = List.filter_map
      (fun sensor ->
        let dist = manhattan_distance sensor.pos sensor.beacon_pos in
        let diff = dist - abs (sensor.pos.y - y0) in
        let min_bound = sensor.pos.x - diff in
        let max_bound = sensor.pos.x + diff  in
          if diff < 0 then
            None
          else
            Some (min_bound, max_bound)
      )
      state.sensors
    in
      Printf.printf "ranges: \n";
      List.iter
        (fun (v1, v2)-> Printf.printf "  (%d, %d)\n" v1 v2)
        ranges;

    let ranges2 = reduce_ranges (Array.of_list ranges) in
      Printf.printf "ranges2: \n";
      Array.iter
        (fun (v1, v2)-> Printf.printf "  (%d, %d)\n" v1 v2)
        ranges2;

    let invalid_position_count =
      (Array.fold_left (fun total (a,b) -> total + (b - a + 1)) 0 ranges2) - beacons_in_y0_count
    in
      Printf.printf "\nthere are %d positions where a beacon cannot be present.\n" invalid_position_count


let test_range state y0 (min_x, max_x) =
  (* |x - x0| + |y - 10| <= dist *)
  (* x0 - dist + abs (y0 - 10) <= x <= x0 + dist - abs (y0 - 10) *)
  let ranges = List.filter_map
    (fun sensor ->
      let dist = manhattan_distance sensor.pos sensor.beacon_pos in
      let diff = dist - abs (sensor.pos.y - y0) in
      let min_bound = sensor.pos.x - diff in
      let max_bound = sensor.pos.x + diff  in
        if diff < 0 then
          None
        else
          Some (min_bound, max_bound)
    )
    state.sensors
  in
  let ranges2 = reduce_ranges (Array.of_list ranges) in
    Array.iter (fun (a, b) ->
      (* if !(a <= min_x && b >= max_x) then *)
      if a > min_x || b < max_x then
      begin
        Printf.printf "  y0 = %d: (%d, %d)\n" y0 a b;

        if b < max_x then
          Printf.printf "    possible tuning freq.: %d\n" ((b + 1)* 4000000 + y0);
      end
    )
    ranges2

let part2 filename =
  let lines = read_file_lines filename in
  let pairs =
    List.map (fun line ->
      parse "Sensor at x=%d, y=%d: closest beacon is at x=%d, y=%d" line read_points
    ) lines
  in
  let state =
    { sensors = []
    ; beacons = []
    }
  in
  let state = List.fold_left
    (fun result (sensor_pt, beacon_pt)-> 
      let existing = List.find_opt
        (fun b -> beacon_pt.x == b.x && beacon_pt.y == b.y)
        result.beacons
      in
        match existing with
        | None -> 
          let sensor =
            { pos = sensor_pt
            ; beacon_pos = beacon_pt
            }
          in
            { sensors = sensor :: result.sensors
            ; beacons = beacon_pt :: result.beacons
            }
        | Some b ->
          let sensor =
            { pos = sensor_pt
            ; beacon_pos = b
            }
          in
            { result with sensors = sensor :: result.sensors }
      )
      state
      pairs
  in
  let state =
    { sensors = List.rev state.sensors
    ; beacons = List.rev state.beacons
    }
  in
  let bounds =
    if String.equal filename "./inputs/day15-example.txt" then
      (0, 20)
    else
      (0, 4000000)
  in
  let (a, b) = bounds in
    for y0 = a to b do
      test_range state y0 bounds
    done

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day15-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

