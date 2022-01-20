{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}

module HW0.T4
  ( fac
  , fib
  , map'
  , repeat'
  ) where

import           Data.Function (fix)
import           GHC.Natural   (Natural)

repeat' :: a -> [a]
repeat' x = fix (x :)

map' :: (a -> b) -> [a] -> [b]
map' f = fix map''
  where
    map'' g = \case
      (a : as) -> f a : g as
      []       -> []

fib :: Natural -> Natural
fib = \case
  0 -> 0
  1 -> 1
  n -> fix fib' 0 1 2
    where
      fib' f prev1 prev2 i = if
        | i == n    -> prev1 + prev2
        | otherwise -> f prev2 (prev1 + prev2) (i + 1)

fac :: Natural -> Natural
fac = fix fac'
  where
    fac' f = \case
      0 -> 1
      x -> x * f (x -1)
