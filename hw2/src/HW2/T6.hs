{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HW2.T6
  ( ParseError (..)
  , Parser (..)
  , parseExpr
  , runP
  ) where

import Control.Applicative (many, optional)
import Control.Monad (MonadPlus, mfilter, void)
import Data.Char (digitToInt, isDigit)
import Data.Scientific (scientific, toRealFloat)
import GHC.Base (Alternative, empty, some, (<|>))
import GHC.Num (Natural)

import HW2.T1 (Annotated (..), Except (..))
import HW2.T4 (Expr (..), Prim (..))
import HW2.T5 (ExceptState (..))

newtype ParseError = ErrorAtPos Natural

newtype Parser a = P (ExceptState ParseError (Natural, String) a)
  deriving (Functor, Applicative, Monad)

instance Alternative Parser where
  empty = parseError

  (<|>) (P exec1) (P exec2) = P $ ES \state -> case runES exec1 state of
    Success annotated -> Success annotated
    Error _           -> runES exec2 state

instance MonadPlus Parser

runP :: Parser a -> String -> Except ParseError a
runP (P exec) s = do
  let except = runES exec (0, s)
  case except of
    Error e          -> Error e
    Success (a :# _) -> Success a

parseExpr :: String -> Except ParseError Expr
parseExpr = runP pStart

pStart :: Parser Expr
pStart = do
  expr <- pExpression
  pSkipWs
  pEof
  pure expr

pExpression :: Parser Expr
pExpression = do
  term <- pTerm
  pExpressionPrime term

pExpressionPrime :: Expr -> Parser Expr
pExpressionPrime term1 = do
  opt <- optional pExprOperation
  case opt of
    Nothing -> pure term1
    Just op -> do
      term2 <- pTerm
      let expr = case op of
            '+' -> Op $ Add term1 term2
            '-' -> Op $ Sub term1 term2
            _   -> error "Exception wasn't thrown properly"
      pExpressionPrime expr <|> pure expr

pTerm :: Parser Expr
pTerm = do
  fact <- pFactor
  pTermPrime fact

pTermPrime :: Expr -> Parser Expr
pTermPrime fact1 = do
  opt <- optional pTermOperation
  case opt of
    Nothing -> pure fact1
    Just op -> do
      fact2 <- pFactor
      let expr = case op of
            '*' -> Op $ Mul fact1 fact2
            '/' -> Op $ Div fact1 fact2
            _   -> error "Exception wasn't thrown properly"
      pTermPrime expr <|> pure expr

pFactor :: Parser Expr
pFactor = do
  pSkipWs
  pDouble <|> pParenExpression

pParenExpression :: Parser Expr
pParenExpression = do
  _ <- pLParen
  expr <- pExpression
  _ <- pRParen
  pure expr

--What happens when the string is empty? Error (ErrorAtPos pos)
--How does the parser state change when a character is consumed? (pos + 1, cs)
pChar :: Parser Char
pChar = P $ ES \(pos, s) -> case s of
  []       -> Error (ErrorAtPos pos)
  (c : cs) -> Success (c :# (pos + 1, cs))

pSatisfy :: (Char -> Bool) -> Parser Char
pSatisfy f = P $ ES \(pos, s) -> case s of
  []       -> Error $ ErrorAtPos pos
  (c : cs) -> if f c then Success (c :# (pos + 1, cs)) else Error $ ErrorAtPos pos

parseError :: Parser a
parseError = P $ ES \(pos, _) -> Error (ErrorAtPos pos)

pEof :: Parser ()
pEof = P $ ES \state@(pos, s) -> case s of
  [] -> Success (() :# state)
  _  -> Error $ ErrorAtPos pos

pSkipWs :: Parser ()
pSkipWs = do void $ many $ mfilter (== ' ') pChar

pDouble :: Parser Expr
pDouble = do
  before <- some (mfilter Data.Char.isDigit pChar)
  sep <- optional (pSatisfy (== '.'))
  case sep of
    Just _ -> do
      after <- some (mfilter Data.Char.isDigit pChar)
      pure $ Val $ toDouble (before ++ after) (- length after)
    Nothing -> pure $ Val $ toDouble before 0

toDouble :: String -> Int -> Double
toDouble string pos = toRealFloat $ scientific (stringToInteger string) pos

stringToInteger :: String -> Integer
stringToInteger s = snd $ foldr fold (1, 0) s
  where
    fold :: Char -> (Integer, Integer) -> (Integer, Integer)
    fold char (multiplier, value) = (multiplier * 10, value + (toInteger . digitToInt) char * multiplier)

pExprOperation :: Parser Char
pExprOperation = pTerminalChar \x -> x == '+' || x == '-'

pTermOperation :: Parser Char
pTermOperation = pTerminalChar \x -> x == '*' || x == '/'

pLParen :: Parser Char
pLParen = pTerminalChar (== '(')

pRParen :: Parser Char
pRParen = pTerminalChar (== ')')

pTerminalChar :: (Char -> Bool) -> Parser Char
pTerminalChar f = do
  pSkipWs
  pSatisfy f
