{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TypeOperators #-}

module HW0.T1
  (type (<->)(Iso)
  , assocEither
  , assocPair
  , distrib
  , flipIso
  , runIso
  ) where

data a <-> b = Iso (a -> b) (b -> a)

flipIso :: (a <-> b) -> (b <-> a)
flipIso (Iso f g) = Iso g f

runIso :: (a <-> b) -> (a -> b)
runIso (Iso f _) = f

distrib :: Either a (b, c) -> (Either a b, Either a c)
distrib (Right (b, c)) = (Right b, Right c)
distrib (Left a)       = (Left a, Left a)

assocPair :: (a, (b, c)) <-> ((a, b), c)
assocPair = Iso (\(a, (b, c)) -> ((a, b), c)) (\((a, b), c) -> (a, (b, c)))

assocEither :: Either a (Either b c) <-> Either (Either a b) c
assocEither =
  Iso
    ( \case
        (Left a) -> Left (Left a)
        (Right inner) ->
          case inner of
            (Left b)  -> Left (Right b)
            (Right c) -> Right c
    )
    ( \case
        (Left inner) ->
          case inner of
            (Left a)  -> Left a
            (Right b) -> Right (Left b)
        (Right c) -> Right (Right c)
    )
