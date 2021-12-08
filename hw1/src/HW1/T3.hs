module HW1.T3
  ( Tree (..)
  , tFromList
  , tdepth
  , tinsert
  , tmember
  , tsize
  ) where

data Tree a
  = Leaf
  | Branch (Int, Int) (Tree a) a (Tree a)
  deriving (Show)

tsize :: Tree a -> Int
tsize Leaf                  = 0
tsize (Branch (s, _) _ _ _) = s

tdepth :: Tree a -> Int
tdepth Leaf                  = 0
tdepth (Branch (_, d) _ _ _) = d

tmember :: Ord a => a -> Tree a -> Bool
tmember _ Leaf = False
tmember a (Branch _ l x r) = case a `compare` x of
  LT -> tmember a l
  EQ -> True
  GT -> tmember a r

tinsert :: Ord a => a -> Tree a -> Tree a
tinsert a Leaf = mkBranch Leaf a Leaf
tinsert a (Branch _ l x r) = case a `compare` x of
  LT -> rotate (tinsert a l) x r
  EQ -> mkBranch l x r
  GT -> rotate l x (tinsert a r)

tFromList :: Ord a => [a] -> Tree a
tFromList = foldr tinsert Leaf

mkBranch :: Tree a -> a -> Tree a -> Tree a
mkBranch l a r = Branch (tsize l + 1 + tsize r, max (tdepth l) (tdepth r) + 1) l a r

rotate :: Tree a -> a -> Tree a -> Tree a
rotate l a r
  | tdepth r - tdepth l == 2 = rotateLeft l a r
  | tdepth l - tdepth r == 2 = rotateRight l a r
  | otherwise                = mkBranch l a r

rotateLeft :: Tree a -> a -> Tree a -> Tree a
rotateLeft l a r@(Branch _ rl ra rr)
  | tdepth rr >= tdepth rl = smallRotateLeft l a r
  | otherwise              = smallRotateLeft l a (smallRotateRight rl ra rr)
rotateLeft _ _ _ = error "unreachable tree structure"

rotateRight :: Tree a -> a -> Tree a -> Tree a
rotateRight l@(Branch _ ll la lr) a r
  | tdepth ll >= tdepth lr = smallRotateRight l a r
  | otherwise              = smallRotateRight (smallRotateLeft ll la lr) a r
rotateRight _ _ _ = error "unreachable tree structure"

smallRotateLeft :: Tree a -> a -> Tree a -> Tree a
smallRotateLeft l a (Branch _ rl ra rr) = mkBranch (mkBranch l a rl) ra rr
smallRotateLeft _ _ _                   = error "unreachable tree structure"

smallRotateRight :: Tree a -> a -> Tree a -> Tree a
smallRotateRight (Branch _ ll la lr) a r = mkBranch ll la (mkBranch lr a r)
smallRotateRight _ _ _                   = error "unreachable tree structure"
