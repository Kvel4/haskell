module HW2.T2
  ( distAnnotated
  , distExcept
  , distFun
  , distList
  , distOption
  , distPair
  , distPrioritised
  , distQuad
  , distStream
  , wrapAnnotated
  , wrapExcept
  , wrapFun
  , wrapList
  , wrapOption
  , wrapPair
  , wrapPrioritised
  , wrapQuad
  , wrapStream
  ) where

import HW2.T1 (Annotated (..), Except (..), Fun (..), List (..), Option (..), Pair (..),
               Prioritised (..), Quad (..), Stream (..))

distOption :: (Option a, Option b) -> Option (a, b)
distOption (Some a, Some b) = Some (a, b)
distOption (_, _)           = None

distPair :: (Pair a, Pair b) -> Pair (a, b)
distPair (P a1 a2, P b1 b2) = P (a1, b1) (a2, b2)

distQuad :: (Quad a, Quad b) -> Quad (a, b)
distQuad (Q a1 a2 a3 a4, Q b1 b2 b3 b4) = Q (a1, b1) (a2, b2) (a3, b3) (a4, b4)

distAnnotated :: Semigroup e => (Annotated e a, Annotated e b) -> Annotated e (a, b)
distAnnotated (a :# e1, b :# e2) = (a, b) :# e2 <> e1

distExcept :: (Except e a, Except e b) -> Except e (a, b)
distExcept (Success a, Success b) = Success (a, b)
distExcept (Error e, _)           = Error e
distExcept (_, Error e)           = Error e

distPrioritised :: (Prioritised a, Prioritised b) -> Prioritised (a, b)
distPrioritised (High a, High b)     = High (a, b)
distPrioritised (High a, Medium b)   = High (a, b)
distPrioritised (High a, Low b)      = High (a, b)
distPrioritised (Medium a, High b)   = High (a, b)
distPrioritised (Medium a, Medium b) = Medium (a, b)
distPrioritised (Medium a, Low b)    = Medium (a, b)
distPrioritised (Low a, High b)      = High (a, b)
distPrioritised (Low a, Medium b)    = Medium (a, b)
distPrioritised (Low a, Low b)       = Low (a, b)

distStream :: (Stream a, Stream b) -> Stream (a, b)
distStream (a :> as, b :> bs) = (a, b) :> distStream (as, bs)

distList :: (List a, List b) -> List (a, b)
distList (a :. as, bList) = addAPairs bList <> distList (as, bList)
  where
    addAPairs (b :. bs) = (a, b) :. addAPairs bs
    addAPairs Nil       = Nil
distList (_, _) = Nil

distFun :: (Fun i a, Fun i b) -> Fun i (a, b)
distFun (F fa, F fb) = F $ \i -> (fa i, fb i)

wrapOption :: a -> Option a
wrapOption = Some

wrapPair :: a -> Pair a
wrapPair a = P a a

wrapQuad :: a -> Quad a
wrapQuad a = Q a a a a

wrapAnnotated :: Monoid e => a -> Annotated e a
wrapAnnotated a = a :# mempty

wrapExcept :: a -> Except e a
wrapExcept = Success

wrapPrioritised :: a -> Prioritised a
wrapPrioritised = Low

wrapStream :: a -> Stream a
wrapStream a = a :> wrapStream a

wrapList :: a -> List a
wrapList a = a :. Nil

wrapFun :: a -> Fun i a
wrapFun a = F $ const a
