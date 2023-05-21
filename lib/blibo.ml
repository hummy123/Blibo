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
          Array.sub children (half + 1) (Array.length children - half - 1) )
    in
    let left = Node (prev_keys, prev_values, prev_children) in
    let right = Node (next_keys, next_values, next_children) in
    Node (median_key, median_value, [| left; right |])
  else Node (keys, values, children)

let insert_at_pos arr ins_val pos =
  let prev = Array.sub arr 0 pos in
  let next = Array.sub arr pos (Array.length arr - pos) in
  Array.append [| ins_val |] next |> Array.append prev

let rec insert ins_key ins_val tree =
  let (Node (keys, values, children)) = tree in
  let rec array_ins pos =
    if pos = Array.length keys then (
      if Array.length children = 0 then
        let keys = Array.append keys [| ins_key |] in
        let values = Array.append values [| ins_val |] in
        split_median keys values children
      else
        let (Node (child_keys, child_values, child_children) as child_node) =
          insert ins_key ins_val
            (Array.unsafe_get children (Array.length children - 1))
        in
        if Array.length child_keys = 1 then (
          let keys = Array.append keys child_keys in
          let values = Array.append values child_values in
          let children =
            Array.append children [| Array.unsafe_get child_children 1 |]
          in
          Array.unsafe_set children
            (Array.length children - 2)
            (Array.unsafe_get child_children 0);
          split_median keys values children)
        else
          let children = Array.copy children in
          Array.unsafe_set children (Array.length children - 1) child_node;
          Node (keys, values, children))
    else
      let cur_key = Array.unsafe_get keys pos in
      if ins_key = cur_key then (
        let values = Array.copy values in
        Array.unsafe_set values pos ins_val;
        Node (keys, values, children))
      else if ins_key > cur_key then array_ins (pos + 1)
        (* Implicit: if above if-statements don't match, then cur_key is greater than ins_key. *)
      else if Array.length children = 0 then
        let keys = insert_at_pos keys ins_key pos in
        let values = insert_at_pos values ins_val pos in
        split_median keys values children
      else
        let (Node (child_keys, child_values, child_children) as child) =
          insert ins_key ins_val (Array.unsafe_get children pos)
        in
        if Array.length child_keys = 1 then
          (* Called split_median after recursing, so need to add to current branch. *)
          let keys = insert_at_pos keys (Array.unsafe_get child_keys 0) pos in
          let values =
            insert_at_pos values (Array.unsafe_get child_values 0) pos
          in
          let half_children =
            insert_at_pos children (Array.unsafe_get child_children 1) pos
          in
          let children =
            insert_at_pos half_children (Array.unsafe_get child_children 0) pos
          in
          split_median keys values children
        else
          (* Set child.  *)
          let children = Array.copy children in
          Array.unsafe_set children pos child;
          Node (keys, values, children)
  in
  array_ins 0

let test_tree =
  let arr = [| 1; 2; 3; 4; 5 |] in
  Array.fold_left (fun tree el -> insert el el tree) empty arr
