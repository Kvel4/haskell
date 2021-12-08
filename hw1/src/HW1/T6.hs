{-# LANGUAGE ScopedTypeVariables #-}

module HW1.T6
  ( epart
  , mcat
  ) where

import Data.Foldable (fold)

mcat :: forall a. Monoid a => [Maybe a] -> a
mcat = fold . fold

epart :: forall a b. (Monoid a, Monoid b) => [Either a b] -> (a, b)
epart = foldr unpack (mempty, mempty)
  where
    unpack :: Either a b -> (a, b) -> (a, b)
    unpack (Left a1) (a, b)  = (a1 <> a, b)
    unpack (Right b1) (a, b) = (a, b1 <> b)
