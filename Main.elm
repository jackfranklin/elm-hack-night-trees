
import Html exposing (..)
import Tree
import TreeView
import Debug


myTree =
  let
    tree = Tree.fromList [2, 9, 7, 6, 1]
    folded = Tree.fold (\x acc -> acc + x) 0 tree
    foo = Debug.log "flattened" (Tree.flatten tree)
    sum = Debug.log "sum" (Tree.sum tree)
    contains = Debug.log "contains" (Tree.contains 2 tree)
  in
    tree
  -- Tree.fromList [2, 9, 7, 6, 1]
  --   |> Tree.map (\a -> a * 2)
  -- Tree.fromList [2, 9, 7, 6, 1]
  -- Tree.singleton 8
  --   |> Tree.insert 2
  --   |> Tree.insert 3
  --   |> Tree.insert 9
  --   |> Tree.insert 11
  --   |> Tree.insert 5


main : Html
main =
    TreeView.draw 800 myTree
