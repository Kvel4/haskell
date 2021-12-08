{-# LANGUAGE ScopedTypeVariables #-}

module HW1.T5
  ( joinWith
  , splitOn
  ) where

import Data.List.NonEmpty (NonEmpty (..), (<|))

splitOn :: forall a. Eq a => a -> [a] -> NonEmpty [a]
splitOn sep = foldr split ([] :| [])
  where
    split :: a -> NonEmpty [a] -> NonEmpty [a]
    split s (x :| xs)
      | s == sep  = [] <| (x :| xs)
      | otherwise = (s : x) :| xs

joinWith :: forall a. a -> NonEmpty [a] -> [a]
joinWith sep arr = tail $ foldr join [] arr
  where
    join :: [a] -> [a] -> [a]
    join token joined = sep : token ++ joined
