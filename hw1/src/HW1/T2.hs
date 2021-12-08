module HW1.T2
  ( N (..)
  , nEven
  , nFromNatural
  , nOdd
  , nToNum
  , ncmp
  , ndiv
  , nmod
  , nmult
  , nplus
  , nsub
  ) where

import GHC.Natural (Natural)

data N = Z | S N
  deriving (Show)

nplus :: N -> N -> N
nplus a Z     = a
nplus a (S b) = S $ a `nplus` b

nmult :: N -> N -> N
nmult _ Z     = Z
nmult a (S b) = a `nplus` (a `nmult` b)

nsub :: N -> N -> Maybe N
nsub a Z         = Just a
nsub Z _         = Nothing
nsub (S a) (S b) = a `nsub` b

ncmp :: N -> N -> Ordering
ncmp a b = case a `nsub` b of
  Nothing -> LT
  Just Z  -> EQ
  _       -> GT

nFromNatural :: Natural -> N
nFromNatural 0 = Z
nFromNatural n = S $ nFromNatural $ n - 1

nToNum :: Num a => N -> a
nToNum Z     = 0
nToNum (S n) = 1 + nToNum n

nEven :: N -> Bool
nEven Z     = True
nEven (S a) = nOdd a

nOdd :: N -> Bool
nOdd Z     = False
nOdd (S a) = nEven a

ndiv :: N -> N -> N
ndiv _ Z = error "zero division"
ndiv a b = subB (a `nsub` b) Z
  where
    subB :: Maybe N -> N -> N
    subB (Just a1) cnt = subB (a1 `nsub` b) (S cnt)
    subB Nothing cnt   = cnt

nmod :: N -> N -> N
nmod _ Z = error "zero division"
nmod a b = subB a
  where
    subB :: N -> N
    subB a1 = maybe a1 subB (a1 `nsub` b)
