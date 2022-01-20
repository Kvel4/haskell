{-# LANGUAGE ScopedTypeVariables #-}

module HW0.T5
  ( Nat
  , nFromNatural
  , nToNum
  , nmult
  , nplus
  , ns
  , nz
  ) where

import GHC.Natural (Natural, naturalToInt)

type Nat a = (a -> a) -> a -> a

nz :: Nat a
nz _ a = a

ns :: Nat a -> Nat a
ns nat f a = f (nat f a)

nplus :: Nat a -> Nat a -> Nat a
nplus nat1 nat2 f a = nat1 f $ nat2 f a

nmult :: Nat a -> Nat a -> Nat a
nmult nat1 nat2 f = nat1 (nat2 f)

nFromNatural :: forall a. Natural -> Nat a
nFromNatural n f a = applyNtimes n where
  applyNtimes ::  Natural -> a
  applyNtimes 0      = a
  applyNtimes number = f (applyNtimes (number - 1))

-- ((a -> a) -> a -> a) -> a
nToNum :: Num a => Nat a -> a
nToNum f = f (+ 1) 0
