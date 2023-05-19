type ('key, 'value) tree =
  | Leaf
  | Node of 'key array * 'value array * ('key, 'value) tree array

let rec mem_opt find_key tree =
  match tree with
  | Leaf -> None
  | Node (keys, values, children) ->
      let rec array_search pos =
        if pos = Array.length keys then
          mem_opt find_key
            (Array.unsafe_get children (Array.length children - 1))
        else
          let cur_key = Array.unsafe_get keys pos in
          if cur_key = find_key then Some (Array.unsafe_get values pos)
          else if cur_key < find_key then array_search (pos + 1)
          else if pos = 0 then mem_opt find_key (Array.unsafe_get children 0)
          else mem_opt find_key (Array.unsafe_get children (pos - 1))
      in
      array_search 0

let test_tree = Node ([| 1; 2 |], [| 'a'; 'b' |], [| Leaf; Leaf |])
let x = mem_opt 1 test_tree
