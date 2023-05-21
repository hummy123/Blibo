type ('key, 'value) tree =
  | Node of 'key array * 'value array * ('key, 'value) tree array

let max_children = 3

(* An empty B-Tree *)
let empty = Node ([||], [||], [||])

let rec fold f state tree =
  let (Node (keys, values, children)) = tree in
  let rec for_all_children state pos =
    if pos = Array.length keys then fold f state (Array.unsafe_get children pos)
    else
      let cur_key = Array.unsafe_get keys pos in
      let cur_val = Array.unsafe_get values pos in
      let state =
        if Array.length children > 0 then
          fold f state (Array.unsafe_get children pos)
        else state
      in
      let state = f state cur_key cur_val in
      for_all_children state (pos + 1)
  in
  for_all_children state 0

let is_empty tree =
  let (Node (keys, _, _)) = tree in
  Array.length keys = 0

let to_list tree = fold (fun lst key value -> (key, value) :: lst) [] tree

let rec find find_key tree =
  let (Node (keys, values, children)) = tree in
  let rec array_search pos =
    if pos = Array.length keys then
      if Array.length children = 0 then None
      else find find_key (Array.unsafe_get children pos)
    else
      let cur_key = Array.unsafe_get keys pos in
      if find_key = cur_key then Some (Array.unsafe_get values pos)
      else if find_key > cur_key then array_search (pos + 1)
        (* Implicit: if above if-statements don't match, then cur_key is greater than find_key. *)
      else if Array.length children = 0 then None
      else find find_key (Array.unsafe_get children pos)
  in
  array_search 0

let split_median keys values children =
  if Array.length keys = max_children then
    let half = max_children / 2 in
    let prev_keys = Array.sub keys 0 half in
    let next_keys = Array.sub keys (half + 1) (Array.length keys - half - 1) in
    let median_key = [| Array.unsafe_get keys half |] in

    let prev_values = Array.sub values 0 half in
    let next_values =
      Array.sub values (half + 1) (Array.length keys - half - 1)
    in
    let median_value = [| Array.unsafe_get values half |] in

    let prev_children, next_children =
      if Array.length children = 0 then ([||], [||])
      else
        ( Array.sub children 0 half,
          Array.sub children half (Array.length keys - half - 1) )
    in
    let left = Node (prev_keys, prev_values, prev_children) in
    let right = Node (next_keys, next_values, next_children) in
    Node (median_key, median_value, [| left; right |])
  else Node (keys, values, children)

let rec insert ins_key ins_val tree =
  let (Node (keys, values, children)) = tree in
  let rec array_ins pos =
    if pos = Array.length keys then
      if Array.length children = 0 then
        let keys = Array.append keys [| ins_key |] in
        let values = Array.append values [| ins_val |] in
        (* Must check if number of keys is above maximum and split if so. *)
        split_median keys values children
      else
        let (Node (keys, values, children)) =
          insert ins_key ins_val (Array.unsafe_get children pos)
        in
        split_median keys values children
    else
      let cur_key = Array.unsafe_get keys pos in
      if ins_key = cur_key then (
        let values = Array.copy values in
        Array.unsafe_set values pos ins_val;
        Node (keys, values, children))
      else if ins_key > cur_key then array_ins (pos + 1)
        (* Implicit: if above if-statements don't match, then cur_key is greater than ins_key. *)
      else if Array.length children = 0 then
        let prev_keys = Array.sub keys 0 pos in
        let next_keys = Array.sub keys pos (Array.length keys - pos) in
        let keys =
          Array.append [| ins_key |] next_keys |> Array.append prev_keys
        in
        let prev_values = Array.sub values 0 pos in
        let next_values = Array.sub values pos (Array.length values - pos) in
        let values =
          Array.append [| ins_val |] next_values |> Array.append prev_values
        in
        split_median keys values children
      else
        let (Node (keys, values, children)) =
          insert ins_key ins_val (Array.unsafe_get children pos)
        in
        split_median keys values children
  in
  array_ins 0

let test_tree =
  let arr = [| 1; 0 |] in
  Array.fold_left (fun tree el -> insert el el tree) empty arr
