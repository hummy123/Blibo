type ('key, 'value) tree =
  | Node of 'key array * 'value array * ('key, 'value) tree array

let empty_arr = [||]

(* An empty B-Tree *)
let empty = Node ([||], [||], [||])

let rec fold f state tree =
  let (Node (keys, values, children)) = tree in
  let rec for_all_children state pos =
    if pos = Array.length keys then fold f state (Array.unsafe_get children pos)
    else
      let cur_key = Array.unsafe_get keys pos in
      let cur_val = Array.unsafe_get values pos in
      let state = fold f state (Array.unsafe_get children pos) in
      let state = f state cur_key cur_val in
      for_all_children state (pos + 1)
  in
  for_all_children state 0

let to_list tree = fold (fun lst key value -> (key, value) :: lst) [] tree
let max_children = 3

let rec find find_key tree =
  let (Node (keys, values, children)) = tree in
  let rec array_search pos =
    if pos = Array.length keys then
      if Array.length children = 0 then None
      else find find_key (Array.unsafe_get children (Array.length children - 1))
    else
      let cur_key = Array.unsafe_get keys pos in
      if cur_key = find_key then Some (Array.unsafe_get values pos)
      else if cur_key < find_key then array_search (pos + 1)
        (* Implicit: if above if-statements don't match, then cur_key is greater than find_key. *)
      else if Array.length children = 0 then None
      else if pos = 0 then find find_key (Array.unsafe_get children 0)
      else find find_key (Array.unsafe_get children (pos - 1))
  in
  array_search 0

let is_empty tree =
  let (Node (keys, _, _)) = tree in
  Array.length keys = 0

let rec insert ins_key ins_value tree =
  let (Node (keys, values, children)) = tree in
  let rec array_search pos =
    if pos = Array.length keys then
      let keys = Array.append keys [| ins_key |] in
      let values = Array.append values [| ins_value |] in

      Node (keys, values, children)
    else
      let cur_key = Array.unsafe_get keys pos in
      if cur_key = ins_key then
        (* Only need to update value, because key already exists. *)
        let new_values_start = Array.sub values 0 pos in
        let new_values_end = Array.sub values pos (Array.length values - pos) in
        let new_values =
          Array.append [| ins_value |] new_values_end
          |> Array.append new_values_start
        in
        Node (keys, new_values, children)
      else if cur_key < ins_key then array_search (pos + 1)
        (* Implicit: if above if-statements don't match, then cur_key is greater than ins_key. *)
      else if pos = 0 then
        (* Must rebalance after insert call. *)
        let ins_child =
          insert ins_key ins_value (Array.unsafe_get children 0)
        in
        let next_children = Array.sub children 1 (Array.length children - 1) in
        let children = Array.append [| ins_child |] next_children in
        Node (keys, values, children)
      else
        (* Must rebalance after insert call. *)
        (* This case is impossible to reach for binary trees (having two children) *)
        let prev_children = Array.sub children 0 pos in
        let next_children =
          Array.sub children (pos + 1) (Array.length children - pos)
        in
        let ins_child =
          insert ins_key ins_value (Array.unsafe_get children (pos - 1))
        in
        let children =
          Array.append [| ins_child |] next_children
          |> Array.append prev_children
        in
        Node (keys, values, children)
  in
  array_search 0

let gen_test_tree =
  let lst =
    [|
      (5, "5");
      (4, "4");
      (6, "6");
      (3, "3");
      (7, "7");
      (7, "777");
      (3, "333");
      (5, "555");
    |]
  in
  Array.fold_left (fun tree (key, value) -> insert key value tree) empty lst
