(* 
   Only have one case for B-Tree data type.
   To detect whether node is child, just check if last array (recursive part)
   is empty. If it is, then this is a leaf; else, it is a branch.

   The elements are ordered, for (max_children = 3), by:
     child[0] -> key/value[0] -> child[1] -> key/value[1] -> child[2]
 *)
type ('key, 'value) tree =
  | Node of 'key array * 'value array * ('key, 'value) tree array

let max_children = 3

(* An empty B-Tree *)
let empty = Node ([||], [||], [||])

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

type search = | Exact of int | Closest of int

(* let binsearch key arr = *)


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

type insert = DidSplit | DidNotSplit

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

    (* Todo: Error might be below. *)
    let prev_children, next_children =
      if Array.length children = 0 then ([||], [||])
      else
        ( Array.sub children 0 (half + 1),
          Array.sub children (half + 1) (Array.length children - half - 1) )
    in
    let left = Node (prev_keys, prev_values, prev_children) in
    let right = Node (next_keys, next_values, next_children) in
    (Node (median_key, median_value, [| left; right |]), DidSplit)
  else (Node (keys, values, children), DidNotSplit)

let insert_at_pos arr ins_val pos =
  let prev = Array.sub arr 0 pos in
  let next = Array.sub arr pos (Array.length arr - pos) in
  Array.append [| ins_val |] next |> Array.append prev

let rec insert_internal ins_key ins_val tree =
  let (Node (keys, values, children)) = tree in
  let rec array_ins pos : ('a, 'b) tree * insert =
    if pos = Array.length keys then (
      if Array.length children = 0 then
        let keys = Array.append keys [| ins_key |] in
        let values = Array.append values [| ins_val |] in
        split_median keys values children
      else
        let ( (Node (child_keys, child_values, child_children) as child_node),
              did_split ) =
          insert_internal ins_key ins_val
            (Array.unsafe_get children (Array.length children - 1))
        in
        match did_split with
        | DidSplit ->
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
            (Node (keys, values, children), DidNotSplit))
    else
      let cur_key = Array.unsafe_get keys pos in
      if ins_key = cur_key then (
        let values = Array.copy values in
        Array.unsafe_set values pos ins_val;
        (Node (keys, values, children), DidNotSplit))
      else if ins_key > cur_key then array_ins (pos + 1)
        (* Implicit: if above if-statements don't match, then cur_key is greater than ins_key. *)
      else if Array.length children = 0 then
        let keys = insert_at_pos keys ins_key pos in
        let values = insert_at_pos values ins_val pos in
        split_median keys values children
      else
        let ( (Node (child_keys, child_values, child_children) as child),
              did_split ) =
          insert_internal ins_key ins_val (Array.unsafe_get children pos)
        in
        match did_split with
        | DidSplit ->
            (* Called split_median after recursing, so need to add to current branch. *)
            let keys = insert_at_pos keys (Array.unsafe_get child_keys 0) pos in
            let values =
              insert_at_pos values (Array.unsafe_get child_values 0) pos
            in
            let half_children =
              insert_at_pos children (Array.unsafe_get child_children 1) pos
            in
            let children =
              insert_at_pos half_children
                (Array.unsafe_get child_children 0)
                pos
            in
            split_median keys values children
        | DidNotSplit ->
            (* Set child.  *)
            let children = Array.copy children in
            Array.unsafe_set children pos child;
            (Node (keys, values, children), DidNotSplit)
  in
  array_ins 0

let insert key value tree =
  let tree, _ = insert_internal key value tree in
  tree

let test_tree =
  let rec ins num tree =
    if num = 16 then tree else ins (num + 1) (insert num "" tree)
  in
  ins 0 empty

let _ =
  Node
    ( [| 7 |],
      [| "" |],
      [|
        Node
          ( [| 3 |],
            [| "" |],
            [|
              Node
                ( [| 1 |],
                  [| "" |],
                  [|
                    Node ([| 0 |], [| "" |], [||]);
                    Node ([| 2 |], [| "" |], [||]);
                  |] );
              Node
                ( [| 5 |],
                  [| "" |],
                  [|
                    Node ([| 4 |], [| "" |], [||]);
                    Node ([| 6 |], [| "" |], [||]);
                  |] );
            |] );
        Node
          ( [| 11 |],
            [| "" |],
            [|
              Node
                ( [| 9 |],
                  [| "" |],
                  [|
                    Node ([| 8 |], [| "" |], [||]);
                    Node ([| 10 |], [| "" |], [||]);
                  |] );
              Node
                ( [| 13 |],
                  [| "" |],
                  [|
                    Node ([| 12 |], [| "" |], [||]);
                    Node ([| 14; 15 |], [| ""; "" |], [||]);
                  |] );
            |] );
      |] )
