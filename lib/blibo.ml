(*
    Only have one case for B-Tree data type.
    To detect whether node is child, just check if last array (recursive part)
    is empty. If it is, then this is a leaf; else, it is a branch.

    The elements are ordered, for (max_children = 3), by:
      child[0] -> key/value[0] -> child[1] -> key/value[1] -> child[2]
*)
type ('key, 'value) tree =
  | Node of 'key array * 'value array * ('key, 'value) tree array

let max_children = 16

(* Hoping there will be fewer memory allocations by declaring empty array/tuple just once. *)
let empty_arr = [||]
let empty_arr_tuple = (empty_arr, empty_arr)

(* An empty B-Tree *)
let empty = Node (empty_arr, empty_arr, empty_arr)

(* Traverse through array following order described at top comment on file. *)
let rec fold f state tree =
  let (Node (keys, values, children)) = tree in
  let rec for_all_children state pos =
    if pos = Array.length keys then
      if Array.length children > 0 then
        fold f state (Array.unsafe_get children pos)
      else state
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

(*
    Binary search on array, intended for use on key array to find index of key.
    If key is not in array, return an index close to where the key should be.
    Caller should then check if the element at the returned index is equal to find_key.
*)
let binary_search find_key arr =
  let rec search low high =
    if high >= low then
      let mid = low + ((high - low) / 2) in
      let mid_key = Array.unsafe_get arr mid in
      if mid_key = find_key then mid
      else if mid_key < find_key then search (low + 1) high
      else search low (mid - 1)
    else
      (* We want to return the closest greater than find_key if find_key is not found. *)
      max high low
  in
  search 0 (Array.length arr - 1)

(* Find a key in a tree if it exists. Returns an option.
   Internally, uses binary_search on array to find correct key or index of child to check. *)
let rec find find_key tree =
  let (Node (keys, values, children)) = tree in
  let idx = binary_search find_key keys in
  if idx = Array.length keys then
    if Array.length children = 0 then None
    else find find_key (Array.unsafe_get children idx)
  else
    let cur_key = Array.unsafe_get keys idx in
    if find_key = cur_key then Some (Array.unsafe_get values idx)
    else if Array.length children = 0 then None
    else if find_key > cur_key then
      find find_key (Array.unsafe_get children (idx + 1))
    else find find_key (Array.unsafe_get children idx)

(*
    On insertion, we only want to split at a branch when a leaf below the branch was split.
    So have to send this information across when unwinding recursion.
*)
type insert = DidSplit | DidNotSplit

(* The standard B-Tree splitting/rebalancing algorithm. Nothing special, except that it returns whether the node was split or not. *)
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
      if Array.length children = 0 then empty_arr_tuple
      else
        ( Array.sub children 0 (half + 1),
          Array.sub children (half + 1) (Array.length children - half - 1) )
    in
    let left = Node (prev_keys, prev_values, prev_children) in
    let right = Node (next_keys, next_values, next_children) in
    (Node (median_key, median_value, [| left; right |]), DidSplit)
  else (Node (keys, values, children), DidNotSplit)

(* Insert into a specific index in an array - unused due to incomplete insert_internal function. *)
let insert_at_pos arr ins_val pos =
  let prev = Array.sub arr 0 pos in
  let next = Array.sub arr pos (Array.length arr - pos) in
  Array.append [| ins_val |] next |> Array.append prev

(* The insert function for B-Trees. Right now, it just supports insertion in increasing order: 0, 1, 2, 3, etc. *)
let rec insert_internal ins_key ins_val tree =
  let (Node (keys, values, children)) = tree in
  let idx = binary_search ins_key keys in
  (* Key to insert is greater than all keys in node. *)
  if idx = Array.length keys then (
    if Array.length children = 0 then
      (* If this node is a leaf with no children, just append the key/value to the end and split if needed. *)
      let keys = Array.append keys [| ins_key |] in
      let values = Array.append values [| ins_val |] in
      split_median keys values children
    else
      (* Insert into last child. *)
      let ( (Node (child_keys, child_values, child_children) as child_node),
            did_split ) =
        insert_internal ins_key ins_val
          (Array.unsafe_get children (Array.length children - 1))
      in
      match did_split with
      | DidSplit ->
          (* If below child did split, then add keys and values to end of this node and split if needed. *)
          let keys = Array.append keys child_keys in
          let values = Array.append values child_values in
          let children =
            Array.append children [| Array.unsafe_get child_children 1 |]
          in
          Array.unsafe_set children
            (Array.length children - 2)
            (Array.unsafe_get child_children 0);
          split_median keys values children
      | DidNotSplit ->
          let children = Array.copy children in
          Array.unsafe_set children (Array.length children - 1) child_node;
          (Node (keys, values, children), DidNotSplit)
      (* Commented out code for non-append/in-order insert - not relevant right now 
       * and requires rewriting after using binary_search to find index. *))
  else
    (* let cur_key = Array.unsafe_get keys idx in *)
    (* if ins_key = cur_key then  *)
    (*   let values = Array.copy values in *)
    (*   Array.unsafe_set values idx ins_val; *)
    (*   (Node (keys, values, children), DidNotSplit) *)
    (* else if ins_key > cur_key then *)
    (*   if Array.length children = 0 then *)
    (*     let keys = insert_at_pos keys ins_key (idx + 1) in *)
    (*     let values = insert_at_pos values ins_val (idx + 1) in *)
    (*     split_median keys values children *)
    (*   else *)
    (*     let (Node(child_keys, child_values, child_children) as child_node), did_split = *)
    (*       insert_internal ins_key ins_val (Array.unsafe_get children (idx + 1)) *)
    (*     in *)
    (*     match did_split with *)
    (*     | DidSplit -> *)

    (* else *)
    failwith ""

(* else if ins_key > cur_key then  *)
(*   array_ins idx + 1 *)
(*   (* Implicit: if above if-statements don't match, then cur_key is greater than ins_key. *) *)
(* else if Array.length children = 0 then *)
(*   let keys = insert_at_pos keys ins_key idx in *)
(*   let values = insert_at_pos values ins_val idx in *)
(*   split_median keys values children *)
(* else *)
(*   let ( (Node (child_keys, child_values, child_children) as child), *)
(*         did_split ) = *)
(*     insert_internal ins_key ins_val (Array.unsafe_get children pos) *)
(*   in *)
(*   match did_split with *)
(*   | DidSplit -> *)
(*       (* Called split_median after recursing, so need to add to current branch. *) *)
(*       let keys = insert_at_pos keys (Array.unsafe_get child_keys 0) pos in *)
(*       let values = *)
(*         insert_at_pos values (Array.unsafe_get child_values 0) pos *)
(*       in *)
(*       let half_children = *)
(*         insert_at_pos children (Array.unsafe_get child_children 1) pos *)
(*       in *)
(*       let children = *)
(*         insert_at_pos half_children *)
(*           (Array.unsafe_get child_children 0) *)
(*           pos *)
(*       in *)
(*       split_median keys values children *)
(*   | DidNotSplit -> *)
(*       (* Set child.  *) *)
(*       let children = Array.copy children in *)
(*       Array.unsafe_set children pos child; *)
(*       (Node (keys, values, children), DidNotSplit) *)

(* Instead of returning tuple like insert_internal, want public API to return just the tree. *)
let insert key value tree =
  let tree, _ = insert_internal key value tree in
  tree

(* Used for viewing the B-Tree and its order/balancning in utop. *)
let test_tree =
  let rec ins num tree =
    if num = 16 then tree else ins (num + 1) (insert num "" tree)
  in
  ins 0 empty
