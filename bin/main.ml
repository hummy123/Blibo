module MapInt = Map.Make (String)

let build_map () =
  let rec loop num map =
    if num = 10_000 then map
    else
      let key = string_of_int num in
      let map = MapInt.add key num map in
      loop (num + 1) map
  in
  loop 0 MapInt.empty

let build_btree () =
  let rec loop num tree =
    if num = 10_000 then tree
    else
      let key = string_of_int num in
      let tree = Blibo.insert key num tree in
      loop (num + 1) tree
  in
  loop 0 Leaf

let () =
  Printf.printf "B-Tree:";
  let start_time = Sys.time () in
  let _ = build_btree () in
  let end_time = Sys.time () in
  let difference = end_time -. start_time in
  Printf.printf "\n%f\n\n" difference;

  Printf.printf "Map:";
  let start_time = Sys.time () in
  let _ = build_map () in
  let end_time = Sys.time () in
  let difference = end_time -. start_time in
  Printf.printf "\n%f\n" difference
