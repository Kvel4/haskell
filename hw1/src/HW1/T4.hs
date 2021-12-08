module HW1.T4
  ( tfoldr
  , treeToList
  ) where

import HW1.T3 (Tree (..))

tfoldr :: (a -> b -> b) -> b -> Tree a -> b
tfoldr _ start Leaf             = start
tfoldr f start (Branch _ l x r) = tfoldr f (f x (tfoldr f start r)) l

treeToList :: Tree a -> [a]
treeToList = tfoldr (:) []
