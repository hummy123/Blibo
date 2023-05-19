type ('key, 'value) tree =
  | Leaf
  | Node of 'key array * 'value array * ('key, 'value) tree array

let rec fold f state = function
  | Leaf -> state
  | Node (keys, values, children) ->
      let rec for_all_children state pos =
        if pos = Array.length keys then
          fold f state (Array.unsafe_get children pos)
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
  match tree with
  | Leaf -> None
  | Node (keys, values, children) ->
      let rec array_search pos =
        if pos = Array.length keys then
          find find_key (Array.unsafe_get children (Array.length children - 1))
        else
          let cur_key = Array.unsafe_get keys pos in
          if cur_key = find_key then Some (Array.unsafe_get values pos)
          else if cur_key < find_key then array_search (pos + 1)
            (* Implicit: if above if-statements don't match, then cur_key is greater than find_key. *)
          else if pos = 0 then find find_key (Array.unsafe_get children 0)
          else find find_key (Array.unsafe_get children (pos - 1))
      in
      array_search 0

let is_empty = function Leaf -> true | Node _ -> false

let get_child_keys = function
  | Node (keys, _, _) -> keys
  | Leaf -> failwith "called get_child_keys on an empty child"

let get_child_values = function
  | Node (_, values, _) -> Some values
  | Leaf -> failwith "called get_child_values on an empty child"

let get_child_children = function
  | Node (_, _, children) -> Some children
  | Leaf -> failwith "called get_child_children on an empty child"

let rec insert ins_key ins_value tree =
  match tree with
  | Leaf -> Node ([| ins_key |], [| ins_value |], [| Leaf; Leaf |])
  | Node (keys, values, children) ->
      let rec array_search pos =
        if pos = Array.length keys then
          (* Must absorb after this if needed - but no absorb logic yet. *)
          (* Must also update calling tree with child returned from insert. *)
          (* Must also update correct child. *)
          let ins_child =
            insert ins_key ins_value
              (Array.unsafe_get children (Array.length children - 1))
          in
          let prev_children =
            Array.sub children 0 (Array.length children - 1)
          in
          let children = Array.append prev_children [| ins_child |] in
          Node (keys, values, children)
        else
          let cur_key = Array.unsafe_get keys pos in
          if cur_key = ins_key then
            (* Only need to update value, because key already exists. *)
            let new_values_start = Array.sub values 0 pos in
            let new_values_end =
              Array.sub values pos (Array.length values - pos)
            in
            let new_values =
              Array.append values new_values_start
              |> Array.append [| ins_value |]
              |> Array.append new_values_end
            in
            Node (keys, new_values, children)
          else if cur_key < ins_key then array_search (pos + 1)
            (* Implicit: if above if-statements don't match, then cur_key is greater than ins_key. *)
          else if pos = 0 then
            (* Must rebalance after insert call. *)
            let ins_child =
              insert ins_key ins_value (Array.unsafe_get children 0)
            in
            let next_children =
              Array.sub children 1 (Array.length children - 1)
            in
            let children = Array.append [| ins_child |] next_children in
            Node (keys, values, children)
          else
            (* Must rebalance after insert call. *)
            (* Must also specify previous and next children. *)
            let ins_child =
              insert ins_key ins_value (Array.unsafe_get children (pos - 1))
            in
            Node (keys, values, [| ins_child |])
      in
      array_search 0

let gen_test_tree =
  let lst =
    [
      (1, "1");
      (100, "100");
      (20, "20");
      (40, "40");
      (80, "80");
      (60, "60");
      (50, "50");
    ]
  in
  List.fold_left (fun tree (key, value) -> insert key value tree) Leaf lst
