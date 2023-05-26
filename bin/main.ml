let build_avl () =
  let rec loop num map =
    if num = 1000_000 then map
    else
      let map = Avl.add num num map in
      loop (num + 1) map
  in
  loop 0 Avl.E

let map_fold map = Avl.fold (fun _ _ _ -> ()) () map

let build_btree () =
  let rec loop num tree =
    if num = 1000_000 then tree
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
  let map = build_avl () in
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
  let _ = Blibo.find 5000 btree in
  let end_time = Sys.time () in
  let difference = end_time -. start_time in
  Printf.printf "%f\n" difference;

  Printf.printf "Map:\n";
  let start_time = Sys.time () in
  let _ = Avl.find_opt 5000 map in
  let end_time = Sys.time () in
  let difference = end_time -. start_time in
  Printf.printf "%f\n" difference
