type ('a, 'b) tree =
  | Leaf
  | Node of 'a array * 'b array * ('a, 'b) tree array

type ('a, 'b) t = ('a, 'b) tree


