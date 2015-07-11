type color = Red | Black
type 'a rbnode = 
  | Node of color * 'a * 'a rbnode * 'a rbnode
  | Nil
type 'a rbtree = {root : 'a rbnode; sort : 'a -> 'a -> bool}

let rb_find tree key = 
  let {root = root; sort = sort} = tree in
  let rec rb_find' = function
    | Nil -> false
    | Node (_, key', left, right) ->
      if sort key key' then rb_find' left
      else if sort key' key then rb_find' right
      else true
  in
  rb_find' root

let rebalance = function
  | Node (Black, z, Node (Red, y, Node (Red, x, a, b), c), d)
  | Node (Black, z, Node (Red, x, a, Node (Red, y, b, c)), d)
  | Node (Black, x, a, Node (Red, y, b, Node (Red, z, c, d)))
  | Node (Black, x, a, Node (Red, z, Node (Red, y, b, c), d))
    -> Node (Red, y, Node (Black, x, a, b), Node (Black, z, c, d))
  | o -> o
  
let rb_insert tree key =
  let {root = root; sort = sort} = tree in
  let rec rb_insert' = function
    | Nil -> Node (Red, key, Nil, Nil)
    | (Node (color, key', left, right)) as node ->
        if sort key key' then
          rebalance (Node (color, key', rb_insert' left, right))
        else if sort key' key then
          rebalance (Node (color, key', left, rb_insert' right))
        else node
  in
  {tree with root =
    match rb_insert' root with
    | Node (Red, key, left, right) -> Node (Black, key, left, right)
    | o -> o}

let make_rbtree sort = {root = Nil; sort = sort}