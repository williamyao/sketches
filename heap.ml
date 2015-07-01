type ('a, 'b) node =
  | Node of 'a * 'b * ('a, 'b) node * ('a, 'b) node
  | Nil

type ('a, 'b) heap = {data : ('a, 'b) node;
                      compare : 'a -> 'a -> bool}

type direction = Left | Right


type 'a cons =
  | Cons of 'a * ('a cons) ref
  | Nil

type 'a queue = {head : ('a cons) ref;
                 tail : ('a cons) ref}

let mkqueue () = {head = ref Nil; tail = ref Nil}

let enqueue queue obj =
  let new_cons = Cons (obj, ref Nil) in
  let {head = head; tail = tail} = queue in
  match !head, !tail with
  | Nil, Nil            -> (head := new_cons; tail := new_cons)
  | _,   Cons (_, next) -> (next := new_cons; tail := new_cons)
  | _,   _              -> raise Exit

let dequeue queue =
  let {head = head; tail = tail} = queue in
  match !head, !tail with
  | Nil, _ -> raise Exit
  | ((Cons (i, _)) as h), t when h = t -> (head := Nil; tail := Nil; i)
  | (Cons (i, next)), _ -> (head := !next; i)

let qhas queue =
  match !(queue.head) with
  | Nil -> false
  | _   -> true

let mkheap compare = {data = Nil; compare = compare}

let find_empty heap =
  let data = heap.data in
  let queue = mkqueue () in
  let rec find_empty' () =
    let node, path = dequeue queue in
    match node with
    | Node (_, _, left, right) ->
      begin
        enqueue queue (left, Left :: path);
	enqueue queue (right, Right :: path);
	find_empty' ()
      end
    | Nil                      -> List.rev path
  in
  begin
    enqueue queue (data, []);
    find_empty' ()
  end
