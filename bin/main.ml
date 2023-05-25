module MapString = Map.Make (Int)

let build_map () =
  let rec loop num map =
    if num = 10_000 then map
    else
      let map = MapString.add num num map in
      loop (num + 1) map
  in
  loop 0 MapString.empty

let map_fold map = MapString.fold (fun _ _ _ -> ()) map ()

let build_btree () =
  let rec loop num tree =
    if num = 10_000 then tree
    else
      let tree = Blibo.insert num num tree in
      loop (num + 1) tree
  in
  loop 0 Blibo.empty

let btree_fold tree = Blibo.fold (fun _ _ _ -> ()) () tree

let () =
  Printf.printf "Insert:\n";
  Printf.printf "\nB-Tree:\n";
  let start_time = Sys.time () in
  let btree = build_btree () in
  let end_time = Sys.time () in
  let difference = end_time -. start_time in
  Printf.printf "%f\n" difference;

  Printf.printf "Map:\n";
  let start_time = Sys.time () in
  let map = build_map () in
  let end_time = Sys.time () in
  let difference = end_time -. start_time in
  Printf.printf "%f\n" difference;

  Printf.printf "Fold:\n";
  Printf.printf "B-Tree:\n";
  let start_time = Sys.time () in
  let _ = btree_fold btree in
  let end_time = Sys.time () in
  let difference = end_time -. start_time in
  Printf.printf "%f\n" difference;

  Printf.printf "Map:\n";
  let start_time = Sys.time () in
  let _ = map_fold map in
  let end_time = Sys.time () in
  let difference = end_time -. start_time in
  Printf.printf "%f\n" difference;

  Printf.printf "Find:\n";
  Printf.printf "B-Tree:\n";
  let start_time = Sys.time () in
  let _ = Blibo.find 3491 btree in
  let end_time = Sys.time () in
  let difference = end_time -. start_time in
  Printf.printf "%f\n" difference;

  Printf.printf "Map:\n";
  let start_time = Sys.time () in
  let _ = MapString.find 3491 map in
  let end_time = Sys.time () in
  let difference = end_time -. start_time in
  Printf.printf "%f\n" difference
