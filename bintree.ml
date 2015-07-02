type 'a node =
  | Node of 'a * 'a node * 'a node
  | Nil

type 'a tree = {data : 'a node; relation : 'a -> 'a -> bool}

let mktree relation = {data = Nil; relation = relation}

let insert tree key =
  let {data = data; relation = relation} = tree in
  let rec insert' = function
    | Nil                              -> Node (key, Nil, Nil)
    | Node (key', left, right) as node ->
        match relation key key', relation key' key with
	| false, false -> node
	| true , false -> Node (key', insert' left, right)
	| _    , _     -> Node (key', left, insert' right)
  in
  {tree with data = insert' data}

let invert tree =
  let rec invert' = function
    | Nil                     -> Nil
    | Node (key, left, right) -> Node (key, invert' right, invert' left)
  in
  {tree with data = invert' tree.data}

let find tree key =
  let {data = data; relation = relation} = tree in
  let rec find' = function
    | Nil                      -> false
    | Node (key', left, right) ->
        match relation key key', relation key' key with
	| false, false -> true
	| true , false -> find' left
	| _    , _     -> find' right
  in
  find' data

let treemin tree =
  let rec treemin' = function
    | Nil -> None
    | Node (key, left, _) ->
        match left with
	| Nil -> Some key
	| _   -> treemin' left
  in
  treemin' tree.data

let treemax tree =
  let rec treemax' = function
    | Nil -> None
    | Node (key, _, right) ->
        match right with
	| Nil -> Some key
	| _   -> treemax' right
  in
  treemax' tree.data