{-# LANGUAGE BlockArguments #-}

module HW3.Parser
  ( parse
  ) where

import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Char (isAlpha, isAlphaNum)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Word (Word8)
import HW3.Base
import Numeric (readHex)
import Text.Megaparsec hiding (parse)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String

parse :: String -> Either (ParseErrorBundle String Void) HiExpr
parse = runParser start "test"

start :: Parser HiExpr
start = between ws eof hiExpr

hiExpr :: Parser HiExpr
hiExpr = makeExprParser hiTerm hiOperators

hiTerm :: Parser HiExpr
hiTerm = do
  open <- optional $ symbol "("
  expr <- case open of
    Just _  -> makeExprParser hiExpr hiOperators <* symbol ")"
    Nothing -> hiValue
  hiArgsSeq $ return expr

hiArgsSeq :: Parser HiExpr -> Parser HiExpr
hiArgsSeq collector = do
  args <- optional hiArgs
  case args of
    Nothing -> hiMarkExpression collector
    Just arg -> do
      marked <- hiMarkExpression $ HiExprApply <$> collector <*> return arg
      hiArgsSeq $ return marked

hiMarkExpression :: Parser HiExpr -> Parser HiExpr
hiMarkExpression expr = do
  marks <- many $ symbol "!"
  case marks of
    [] -> expr
    xs -> foldl (\col _ -> HiExprRun <$> col) expr xs

hiValue :: Parser HiExpr
hiValue =
  HiExprValue
    <$> ( HiValueNumber <$> number
            <|> HiValueFunction <$> hiFun
            <|> HiValueBool <$> bool
            <|> HiValueNull <$ symbol "null"
            <|> HiValueString <$> hiString
            <|> HiValueBytes <$> hiBytes
            <|> HiValueAction HiActionCwd <$ symbol "cwd"
            <|> HiValueAction HiActionNow <$ symbol "now"
        )
    <|> HiExprApply (HiExprValue $ HiValueFunction HiFunList) <$> hiList
    <|> HiExprDict <$> hiDict

hiDict :: Parser [(HiExpr, HiExpr)]
hiDict = symbol "{" *> sepBy hiEntry (symbol ",") <* symbol "}"

hiEntry :: Parser (HiExpr, HiExpr)
hiEntry = try $ do
  expr1 <- hiExpr
  _ <- symbol ":"
  expr2 <- hiExpr
  return (expr1, expr2)

hiBytes :: Parser ByteString
hiBytes = symbol "[#" *> bytes <* symbol "#]"

bytes :: Parser ByteString
bytes = B.pack <$> sepEndBy byte space1

byte :: Parser Word8
byte = fst . head . readHex <$> count 2 hexDigitChar

hiList :: Parser [HiExpr]
hiList = symbol "[" *> hiExprSeq <* symbol "]"

hiString :: Parser Text
hiString = T.pack <$> lexeme (char '"' *> manyTill L.charLiteral (char '"'))

bool :: Parser Bool
bool = True <$ symbol "true" <|> False <$ symbol "false"

hiFun :: Parser HiFun
hiFun =
  choice $
    map
      (\fun -> fun <$ symbol (show fun))
      [ HiFunDiv,
        HiFunMul,
        HiFunAdd,
        HiFunAnd,
        HiFunOr,
        HiFunLessThan,
        HiFunGreaterThan,
        HiFunEquals,
        HiFunNotLessThan,
        HiFunNotGreaterThan,
        HiFunNotEquals,
        HiFunIf,
        HiFunLength,
        HiFunToUpper,
        HiFunToLower,
        HiFunReverse,
        HiFunTrim,
        HiFunList,
        HiFunRange,
        HiFunFold,
        HiFunPackBytes,
        HiFunUnpackBytes,
        HiFunEncodeUtf8,
        HiFunDecodeUtf8,
        HiFunZip,
        HiFunUnzip,
        HiFunSerialise,
        HiFunDeserialise,
        HiFunRead,
        HiFunWrite,
        HiFunMkDir,
        HiFunChDir,
        HiFunParseTime,
        HiFunRand,
        HiFunEcho,
        HiFunCount,
        HiFunKeys,
        HiFunValues,
        HiFunInvert,
        HiFunNot,
        HiFunSub
      ]

hiArgs :: Parser [HiExpr]
hiArgs = (char '.' *> hiPointIdentifier) <|> parens hiExprSeq

hiPointIdentifier :: Parser [HiExpr]
hiPointIdentifier =
  fmap (: []) $
    HiExprValue . HiValueString . T.pack . intercalate "-"
      <$> (((:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)) `sepBy` char '-') <* ws

hiExprSeq :: Parser [HiExpr]
hiExprSeq = sepBy hiExpr (symbol ",")

hiOperators :: [[Operator Parser HiExpr]]
hiOperators =
  [ [ InfixL (binaryOperator HiFunMul <$ symbol "*"),
      InfixL (binaryOperator HiFunDiv <$ prefixSymbol "/")
    ],
    [ InfixL (binaryOperator HiFunAdd <$ symbol "+"),
      InfixL (binaryOperator HiFunSub <$ symbol "-")
    ],
    [ InfixN (binaryOperator HiFunEquals <$ symbol "=="),
      InfixN (binaryOperator HiFunNotEquals <$ symbol "/="),
      InfixN (binaryOperator HiFunLessThan <$ prefixSymbol "<"),
      InfixN (binaryOperator HiFunNotLessThan <$ symbol "<="),
      InfixN (binaryOperator HiFunGreaterThan <$ prefixSymbol ">"),
      InfixN (binaryOperator HiFunNotGreaterThan <$ symbol ">=")
    ],
    [InfixR (binaryOperator HiFunAnd <$ symbol "&&")],
    [InfixR (binaryOperator HiFunOr <$ symbol "||")]
  ]

prefixSymbol :: String -> Parser String
prefixSymbol s = (lexeme . try) (symbol s <* notFollowedBy (symbol "="))

binaryOperator :: HiFun -> HiExpr -> HiExpr -> HiExpr
binaryOperator op a b = HiExprApply (HiExprValue $ HiValueFunction op) [a, b]

ws :: Parser ()
ws = space

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: String -> Parser String
symbol = L.symbol ws

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

number :: Parser Rational
number = L.signed ws (toRational <$> lexeme L.scientific)
