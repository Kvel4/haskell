module HW1.T7
  ( DotString (..)
  , Fun (..)
  , Inclusive (..)
  , ListPlus (..)
  ) where

data ListPlus a = a :+ ListPlus a | Last a deriving (Show)

infixr 5 :+

instance Semigroup (ListPlus a) where
  Last a <> b    = a :+ b
  (x :+ xs) <> b = x :+ xs <> b

data Inclusive a b = This a | That b | Both a b

instance (Semigroup a, Semigroup b) => Semigroup (Inclusive a b) where
  This a <> This b       = This $ a <> b
  This a <> Both a1 b1   = Both (a <> a1) b1
  This a <> That b       = Both a b
  That a <> That b       = That $ a <> b
  That b <> Both a1 b1   = Both a1 (b <> b1)
  That b <> This a       = Both a b
  Both a b <> Both a1 b1 = Both (a <> a1) (b <> b1)
  Both a b <> That b1    = Both a (b <> b1)
  Both a b <> This a1    = Both (a <> a1) b

newtype DotString = DS String deriving (Show)

instance Semigroup DotString where
  DS "" <> DS b = DS b
  DS a <> DS "" = DS a
  DS a <> DS b  = DS $ a ++ "." ++ b

instance Monoid DotString where
  mempty = DS ""

newtype Fun a = F (a -> a)

instance Semigroup (Fun a) where
  F f1 <> F f2 = F (f1 . f2)

instance Monoid (Fun a) where
  mempty = F id
