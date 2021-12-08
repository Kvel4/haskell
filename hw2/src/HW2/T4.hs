module HW2.T4
  ( Expr (..)
  , Prim (..)
  , State (..)
  , eval
  , joinState
  , mapState
  , modifyState
  , wrapState
  ) where

import Control.Monad

import HW2.T1 (Annotated (..))

newtype State s a = S {runS :: s -> Annotated s a}

data Prim a
  = Add a a
  | Sub a a
  | Mul a a
  | Div a a
  | Abs a
  | Sgn a

data Expr = Val Double | Op (Prim Expr)

instance Num Expr where
  x + y = Op (Add x y)
  x - y = Op (Sub x y)
  x * y = Op (Mul x y)
  abs x = Op (Abs x)
  signum x = Op (Sgn x)

  fromInteger x = Val (fromInteger x)

instance Fractional Expr where
  x / y = Op (Div x y)

  fromRational x = Val (fromRational x)

instance Functor (State s) where
  fmap = mapState

instance Applicative (State s) where
  pure = wrapState
  p <*> q = Control.Monad.ap p q

instance Monad (State s) where
  m >>= f = joinState (fmap f m)

mapState :: (a -> b) -> State s a -> State s b
mapState f state = S $ \s -> do
  let (a :# s1) = runS state s
  f a :# s1

wrapState :: a -> State s a
wrapState a = S $ \s -> a :# s

joinState :: State s (State s a) -> State s a
joinState ann = S $ \s -> do
  let (ann1 :# s1) = runS ann s
  runS ann1 s1

modifyState :: (s -> s) -> State s ()
modifyState f = S $ \s -> () :# f s

eval :: Expr -> State [Prim Double] Double
eval (Op (Add a b)) = binaryOperation a b (+) Add
eval (Op (Sub a b)) = binaryOperation a b (-) Sub
eval (Op (Mul a b)) = binaryOperation a b (*) Mul
eval (Op (Div a b)) = binaryOperation a b (/) Div
eval (Op (Abs a))   = unaryOperation a abs Abs
eval (Op (Sgn a))   = unaryOperation a signum Sgn
eval (Val val)      = pure val

binaryOperation ::
  Expr ->
  Expr ->
  (Double -> Double -> Double) ->
  (Double -> Double -> Prim Double) ->
  State [Prim Double] Double
binaryOperation a b op constructor = do
  a1 <- eval a
  b1 <- eval b
  modifyState $ \s -> constructor a1 b1 : s
  return $ a1 `op` b1

unaryOperation ::
  Expr ->
  (Double -> Double) ->
  (Double -> Prim Double) ->
  State [Prim Double] Double
unaryOperation a op constructor = do
  a1 <- eval a
  modifyState $ \s -> constructor a1 : s
  return $ op a1
