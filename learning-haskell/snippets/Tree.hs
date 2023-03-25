module Main where

-- Binary Tree with left-first insertion
data Tree a
  = Leaf a
  | Half (Tree a) a
  | Node (Tree a) a (Tree a)
  deriving (Show, Ord, Eq)

instance Functor Tree where
  fmap f tree =
    case tree of 
      (Leaf x)     -> Leaf (f x)
      (Half l x)   -> Half (fmap f l) (f x)
      (Node l x r) -> Node (fmap f l) (f x) (fmap f r)
  
insert :: Ord a => a -> Tree a -> Tree a
insert value tree =
  case tree of
    (Leaf x) -> 
      if value > x
        then Half (Leaf x) value 
        else Half (Leaf value) x
    (Half l x) ->
      if value > x
        then Node l x (Leaf value)
        else Half (insert value l) x
    (Node l x r) ->
      if value > x
        then Node l x (insert value r)
        else Node (insert value l) x r

-- Exploring BFS vs DFS for `elem` function
-- | Returns True if the element `a` is present in the given tree.
elem :: Ord a => a -> Tree a -> Bool
elem = error "not implemented yet"

-- Sample Tree of integers
--        4
--      /  \
--    2     6
--   / \
--  1   3
sample :: Tree Int
sample = 
  Node 
    (Node
      (Leaf 1)
      2
      (Leaf 3)
    )
    4
    (Leaf 6)

main :: IO ()
main = do
  putStrLn "Hello, World!"
