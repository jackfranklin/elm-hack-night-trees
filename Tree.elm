module Tree where


type Tree a
    = Empty
    | Node a (Tree a) (Tree a)


empty : Tree a
empty =
  Empty


singleton : a -> Tree a
singleton val =
  Node val Empty Empty


insert : comparable -> Tree comparable -> Tree comparable
insert newValue tree =
  case tree of
    Empty -> singleton newValue
    Node value leftTree rightTree ->
      if value == newValue then
        tree
      else if value > newValue then
        Node value (insert newValue leftTree) rightTree
      else
        Node value leftTree (insert newValue rightTree)


fromList : List comparable -> Tree comparable
fromList list =
  List.foldl insert empty list


depth : Tree a -> Int
depth tree =
  case tree of
    Empty -> 0
    Node _ leftTree rightTree ->
      1 + max (depth leftTree) (depth rightTree)


map : (a -> b) -> Tree a -> Tree b
map func tree =
   case tree of
     Empty -> Empty
     Node x t1 t2 ->
       Node (func x) (map func t1) (map func t2)

sum : Tree number -> number
sum tree =
  fold (+) 0 tree
  -- case tree of
  --   Empty -> 0
  --   Node x t1 t2 ->
  --     x + (sum t1) + (sum t2)

flatten : Tree a -> List a
flatten tree =
  fold (::) [] tree
  -- case tree of
  --   Empty -> []
  --   Node x t1 t2 ->
  --     (flatten t1) ++ [x] ++ (flatten t2)

contains : a -> Tree a -> Bool
contains x tree =
  fold (\y acc -> y == x || acc) False tree
  -- case tree of
  --   Empty -> False
  --   Node val t1 t2 ->
  --     if val == x then
  --       True
  --     else
  --       (contains x t1) || (contains x t2)

fold : (a -> b -> b) -> b -> Tree a -> b
fold func accumulator tree =
  -- List.foldl func accumulator (flatten tree)
  case tree of
    Empty -> accumulator
    Node x t1 t2 ->
      fold func (fold func (func x accumulator) t1) t2



{-----------------------------------------------------------------

Exercises:

(1) Sum all of the elements of a tree.

       sum : Tree number -> number

(2) Flatten a tree into a list.

       flatten : Tree a -> List a

(3) Check to see if an element is in a given tree.

       contains : a -> Tree a -> Bool

(4) Write a general fold function that acts on trees. The fold
    function does not need to guarantee a particular order of
    traversal.

       fold : (a -> b -> b) -> b -> Tree a -> b

(5) Use "fold" to do exercises 1-3 in one line each. The best
    readable versions I have come up have the following length
    in characters including spaces and function name:
      sum: 16
      flatten: 21
      contains: 45
    See if you can match or beat me! Don't forget about currying
    and partial application!

(6) Can "fold" be used to implement "map" or "depth"?

(7) Try experimenting with different ways to traverse a
    tree: pre-order, in-order, post-order, depth-first, etc.
    More info at: http://en.wikipedia.org/wiki/Tree_traversal

-----------------------------------------------------------------}
