open Utils

let min3 a b c =
  if a > b then (
    if b > c then c else b
  ) else (
    if a > c then c else a
  )

type point_t =
  { x: int
  ; y: int
  ; z: int
  }

type state_t =
  | Unknown
  | Air
  | Steam
  | Lava

type cube_t =
  { x: int
  ; y: int
  ; z: int
  ; mutable free_faces: int
  ; mutable state: state_t
  }

let parse_cube (cube_str: string): cube_t =
  let nums = String.split_on_char ',' cube_str in
    match nums with
    | x :: y :: z :: tl ->
      { x = int_of_string x
      ; y = int_of_string y
      ; z = int_of_string z
      ; free_faces = 6
      ; state = Unknown
      }
    | _ -> failwith "Unreachable: parse_cube"

let are_cubes_connected (c1: cube_t) (c2: cube_t): bool =
  let dx = abs (c1.x - c2.x) in
  let dy = abs (c1.y - c2.y) in
  let dz = abs (c1.z - c2.z) in
    (dx == 0 && dy == 0 && dz == 1) ||
    (dx == 1 && dy == 0 && dz == 0) ||
    (dx == 0 && dy == 1 && dz == 0)

let array_to_string (item_to_string: 'a -> string) (arr: 'a array): string =
  let formater (i: int) (value: 'a) : string =
    if i + 1 == Array.length arr then
      item_to_string value
    else
      (item_to_string value) ^ ", "
  in
    "[" ^ (concats (Array.to_list (Array.mapi formater arr))) ^ "]"

let part1 filename =
  let lines = read_file_lines filename in
  let cubes = List.map parse_cube lines in
  let cubes = Array.of_list cubes in
  let n = Array.length cubes in

    for i = 0 to n - 1 do
      let c1 = cubes.(i) in
      for j = i + 1 to n - 1 do
        let c2 = cubes.(j) in
        if c1.free_faces > 0 && c2.free_faces > 0 && (are_cubes_connected c1 c2) then
        begin
          c1.free_faces <- c1.free_faces - 1;
          c2.free_faces <- c2.free_faces - 1;
        end
      done
    done;

  let area = Array.fold_left (fun acc cube -> acc + cube.free_faces) 0 cubes in
    Printf.printf "area: %d\n" area

let read_upper_bound (cubes: cube_t array): point_t =
  let bound = ref ({ x = -100000; y = -100000; z = -100000 }) in
  let n = Array.length cubes in
    for i = 0 to n - 1 do
      let cube = cubes.(i) in
        bound :=
          { x = max !bound.x cube.x
          ; y = max !bound.y cube.y
          ; z = max !bound.z cube.z
          }
    done;

    !bound

let array_of_cubes_from_bound (b: point_t): cube_t array * int =
  let size = max (b.x + 1) (max (b.y + 1) (b.z + 1)) in
  let default_cube =
    { x = 0
    ; y = 0
    ; z = 0
    ; free_faces = 6
    ; state = Unknown
    }
  in
  let cubes = Array.make (size * size * size) default_cube in
  let idx = ref 0 in
    for i = 0 to size - 1 do
      for j = 0 to size - 1  do
        for k = 0 to size - 1  do
          cubes.(!idx) <-
            { x = i
            ; y = j
            ; z = k
            ; free_faces = 6
            ; state = Air
            };
          incr idx
        done
      done
    done;

    (cubes, size)

let print_possible_cubes (cubes: cube_t array) (size: int): unit =
  let idx = ref 0 in
    for i = 0 to size - 1 do
      Printf.printf "i: %d\n" i;
      for j = 0 to size - 1 do
        for k = 0 to size - 1 do
          begin
          match cubes.(!idx).state with
          | Lava -> Printf.printf "L"
          | Steam -> Printf.printf "S"
          | Air -> Printf.printf "A"
          | Unknown -> Printf.printf "U"
          end;
          incr idx;
        done;
        Printf.printf "\n"
      done;
      Printf.printf "\n"
    done

(* It wasn't need, but this function doesn't solve this case:
    AALLAAA       AALLAAA
    ALAALAA       ALSSLAA
    LAAAALA  ->   LASSSLA
    LAAAAAL       LASSSSL
    LAAAALA       LASSSLA
    LLLALLA       LLLSLLA
       S <-          S
  because it will fill steam to the nearest neighbors leaving
  column of air.

  It could be done by calling this function multiple times.
 *)
let fill_steam_flow (cubes: cube_t array) (size: int): unit =
  let size2 = size * size in
  let get_idx x y z =
    x * size2 + y * size + z
  in
  let add_steam_to_cube x y z =
    if not (x < 0 || x >= size || y < 0 || y >= size || z < 0 || z >= size) then
      let idx = x * size2 + y * size + z in
        if cubes.(idx).state == Air then
          cubes.(idx).state <- Steam
  in
  let add_steam_to_neighbors cube =
    add_steam_to_cube (cube.x - 1) cube.y cube.z;
    add_steam_to_cube (cube.x + 1) cube.y cube.z;
    add_steam_to_cube cube.x (cube.y - 1) cube.z;
    add_steam_to_cube cube.x (cube.y + 1) cube.z;
    add_steam_to_cube cube.x cube.y (cube.z - 1);
    add_steam_to_cube cube.x cube.y (cube.z + 1)
  in
  let is_neighbor_steamed x y z =
    if x < 0 || x >= size || y < 0 || y >= size || z < 0 || z >= size then
      true
    else
      let idx = x * size2 + y * size + z in
        cubes.(idx).state == Steam
  in
  let has_steamed_neighbor cube =
    let n1 = is_neighbor_steamed (cube.x - 1) cube.y cube.z in
    let n2 = is_neighbor_steamed (cube.x + 1) cube.y cube.z in
    let n3 = is_neighbor_steamed cube.x (cube.y - 1) cube.z in
    let n4 = is_neighbor_steamed cube.x (cube.y + 1) cube.z in
    let n5 = is_neighbor_steamed cube.x cube.y (cube.z - 1) in
    let n6 = is_neighbor_steamed cube.x cube.y (cube.z + 1) in
      n1 || n2 || n3 || n4 || n5 || n6
  in
  let rec loop cube_start cube_end =
    for i = cube_start to cube_end do
      for j = cube_start to cube_end do
        let idx = get_idx i j cube_start in
        let cube = cubes.(idx) in
          if cube.state == Air && (has_steamed_neighbor cube) then (
            cube.state <- Steam;
            add_steam_to_neighbors cube
          );

        let idx = get_idx i j cube_end in
        let cube = cubes.(idx) in
          if cube.state == Air && (has_steamed_neighbor cube) then (
            cube.state <- Steam;
            add_steam_to_neighbors cube
          )
      done;

      for k = cube_start to cube_end do
        let idx = get_idx i cube_start k in
        let cube = cubes.(idx) in
          if cube.state == Air && (has_steamed_neighbor cube) then (
            cube.state <- Steam;
            add_steam_to_neighbors cube
          );

        let idx = get_idx i cube_end k in
        let cube = cubes.(idx) in
          if cube.state == Air && (has_steamed_neighbor cube) then (
            cube.state <- Steam;
            add_steam_to_neighbors cube
          )
      done
    done;

    for j = cube_start to cube_end do
      for k = cube_start to cube_end do
        let idx = get_idx cube_start j k in
        let cube = cubes.(idx) in
          if cube.state == Air && (has_steamed_neighbor cube) then (
            cube.state <- Steam;
            add_steam_to_neighbors cube
          );

        let idx = get_idx cube_end j k in
        let cube = cubes.(idx) in
          if cube.state == Air && (has_steamed_neighbor cube) then (
            cube.state <- Steam;
            add_steam_to_neighbors cube
          )
      done
    done;

    if cube_start + 1 < cube_end then
      loop (cube_start + 1) (cube_end - 1)
  in
    loop 0 (size - 1)

let part2 filename =
  let lines = read_file_lines filename in
  let cubes = List.map parse_cube lines in
  let cubes = Array.of_list cubes in
  let bound = read_upper_bound cubes in
  let (possible_cubes, max_cube_size) =
    array_of_cubes_from_bound bound
  in
  let n = Array.length cubes in
    for i = 0 to n - 1 do
      let c1 = cubes.(i) in
      for j = i + 1 to n - 1 do
        let c2 = cubes.(j) in
        if c1.free_faces > 0 && c2.free_faces > 0 && (are_cubes_connected c1 c2) then
        begin
          c1.free_faces <- c1.free_faces - 1;
          c2.free_faces <- c2.free_faces - 1
        end
      done
    done;

  let n2 = Array.length possible_cubes in
    for i = 0 to n - 1 do
      let c1 = cubes.(i) in
      let c2_idx_opt =
        Array.find_index
        (fun cube -> cube.x == c1.x && cube.y == c1.y && cube.z == c1.z)
        possible_cubes
      in
      match c2_idx_opt with
      | Some c2_idx ->
        possible_cubes.(c2_idx).state <- Lava
      | _ -> failwith "Unreachable: cube not found"
    done;

    (* print_possible_cubes possible_cubes max_cube_size; *)
    fill_steam_flow possible_cubes max_cube_size;

    for i = 0 to n2 - 1 do
      let c1 = possible_cubes.(i) in
      if c1.state == Air then
      begin
        for j = 0 to n - 1 do
          let c2 = cubes.(j) in
          if c1.free_faces > 0 && (are_cubes_connected c1 c2) then
          begin
            c1.free_faces <- c1.free_faces - 1
          end
        done
      end
    done;

  let area = Array.fold_left (fun acc cube -> acc + cube.free_faces) 0 cubes in
  let air_area = Array.fold_left
    (fun acc cube ->
      if cube.state = Air then
        acc + (6 - cube.free_faces)
      else
        acc
    )
    0
    possible_cubes
  in
    print_possible_cubes possible_cubes max_cube_size;
    Printf.printf "area: %d\n" area;
    Printf.printf "area - air: %d\n" (area - air_area)

(* Calling the solutions *)

let () =
  let raw_args = Args.read () in
  let parsed = Args.parse raw_args in
  let filename =
    match parsed.file with
    | None -> "./inputs/day18-example.txt"
    | Some name -> name
  in
    if parsed.part == 1 then
      part1 filename
    else if parsed.part == 2 then
      part2 filename

