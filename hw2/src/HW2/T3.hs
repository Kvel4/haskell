module HW2.T3
  ( joinAnnotated
  , joinExcept
  , joinFun
  , joinList
  , joinOption
  ) where

import HW2.T1 (Annotated (..), Except (..), Fun (..), List (..), Option (..))

joinOption :: Option (Option a) -> Option a
joinOption (Some (Some a)) = Some a
joinOption _               = None

joinExcept :: Except e (Except e a) -> Except e a
joinExcept (Success (Success a)) = Success a
joinExcept (Success (Error e))   = Error e
joinExcept (Error e)             = Error e

joinAnnotated :: Semigroup e => Annotated e (Annotated e a) -> Annotated e a
joinAnnotated ((a :# e1) :# e2) = a :# e1 <> e2

joinList :: List (List a) -> List a
joinList (a :. as) = a <> joinList as
joinList Nil       = Nil

joinFun :: Fun i (Fun i a) -> Fun i a
joinFun (F f) = F $ \i -> getFunc (f i) i
  where
    getFunc (F g) = g
