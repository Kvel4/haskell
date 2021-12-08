module HW2.T5
  ( EvaluationError (..)
  , ExceptState (..)
  , eval
  , joinExceptState
  , mapExceptState
  , modifyExceptState
  , throwExceptState
  , wrapExceptState
  ) where

import Control.Monad

import HW2.T1 (Annotated (..), Except (..))
import HW2.T4 (Expr (..), Prim (..))

newtype ExceptState e s a = ES {runES :: s -> Except e (Annotated s a)}

data EvaluationError = DivideByZero

instance Functor (ExceptState e s) where
  fmap = mapExceptState

instance Applicative (ExceptState e s) where
  pure = wrapExceptState

  p <*> q = Control.Monad.ap p q

instance Monad (ExceptState e s) where
  m >>= f = joinExceptState (fmap f m)

mapExceptState :: (a -> b) -> ExceptState e s a -> ExceptState e s b
mapExceptState f state = ES $ \s -> do
  let except = runES state s
  case except of
    Error e           -> Error e
    Success (a :# s1) -> Success (f a :# s1)

wrapExceptState :: a -> ExceptState e s a
wrapExceptState a = ES $ \s -> Success (a :# s)

joinExceptState :: ExceptState e s (ExceptState e s a) -> ExceptState e s a
joinExceptState state = ES $ \s -> do
  let except = runES state s
  case except of
    Error e                -> Error e
    Success (state2 :# s1) -> runES state2 s1

modifyExceptState :: (s -> s) -> ExceptState e s ()
modifyExceptState f = ES $ \s -> Success (() :# f s)

throwExceptState :: e -> ExceptState e s a
throwExceptState e = ES $ \_ -> Error e

eval :: Expr -> ExceptState EvaluationError [Prim Double] Double
eval (Op (Add a b)) = binaryOperation a b (+) False Add
eval (Op (Sub a b)) = binaryOperation a b (-) False Sub
eval (Op (Mul a b)) = binaryOperation a b (*) False Mul
eval (Op (Div a b)) = binaryOperation a b (/) True  Div
eval (Op (Abs a))   = unaryOperation a abs Abs
eval (Op (Sgn a))   = unaryOperation a signum Sgn
eval (Val val)      = pure val

binaryOperation ::
  Expr ->
  Expr ->
  (Double -> Double -> Double) ->
  Bool ->
  (Double -> Double -> Prim Double) ->
  ExceptState EvaluationError [Prim Double] Double
binaryOperation a b op checkError constructor = do
  a1 <- eval a
  b1 <- eval b
  modifyExceptState $ \s -> constructor a1 b1 : s
  if checkError && b1 == 0
    then throwExceptState DivideByZero
    else return $ (a1 `op` b1)

unaryOperation ::
  Expr ->
  (Double -> Double) ->
  (Double -> Prim Double) ->
  ExceptState EvaluationError [Prim Double] Double
unaryOperation a op constructor = do
  a1 <- eval a
  modifyExceptState $ \s -> constructor a1 : s
  return (op a1)
