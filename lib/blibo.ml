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
let max_children = 3

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

let rec insert ins_key ins_val tree =
  let rec ins pos tree =
    let (Node (keys, values, children)) = tree in
    if pos = Array.length keys then
      if Array.length children = 0 then
        let keys = Array.append keys [| ins_key |] in
        let values = Array.append values [| ins_val |] in
        (* Must check if number of keys is above maximum and split if so. *)
        Node (keys, values, children)
      else insert ins_key ins_val (Array.unsafe_get children pos)
    else failwith ""
  in
  ins 0 tree
