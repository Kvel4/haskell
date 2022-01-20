{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleInstances   #-}

module HW3.Evaluator
  ( eval
  ) where

import Codec.Compression.Zlib (bestCompression, compressLevel, compressWith, decompress,
                               defaultCompressParams)
import Codec.Serialise (deserialise, serialise)
import Control.Monad.Except
import qualified Data.ByteString as B
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.Foldable (toList)
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Ratio (denominator, numerator)
import Data.Semigroup (stimes)
import Data.Sequence (Seq (..), (><))
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8', encodeUtf8)
import Data.Time.Clock (addUTCTime, diffUTCTime)
import Data.Word (Word8)
import HW3.Base
import Prelude hiding (toInteger)
import Text.Read (readMaybe)

eval :: HiMonad m => HiExpr -> m (Either HiError HiValue)
eval = runExceptT . evalHiExpr

evalHiExpr :: HiMonad m => HiExpr -> ExceptT HiError m HiValue
evalHiExpr (HiExprValue value) = return value
evalHiExpr (HiExprRun expr) = do
  value <- evalHiExpr expr
  case value of
    HiValueAction action -> lift $ runAction action
    _                    -> throwError HiErrorInvalidArgument
evalHiExpr (HiExprApply expr args) = do
  value <- evalHiExpr expr
  case value of
    HiValueFunction fun    -> evalHiFun fun args
    text@(HiValueString _) -> evalSlice text args
    list@(HiValueList _)   -> evalSlice list args
    bytes@(HiValueBytes _) -> evalSlice bytes args
    dict@(HiValueDict _)   -> evalSlice dict args
    _                      -> throwError HiErrorInvalidFunction
evalHiExpr (HiExprDict entries) = HiValueDict . M.fromList <$> mapM pairToEntry entries

pairToEntry :: HiMonad m => (HiExpr, HiExpr) -> ExceptT HiError m (HiValue, HiValue)
pairToEntry (expr1, expr2) = do
  value1 <- evalHiExpr expr1
  value2 <- evalHiExpr expr2
  return (value1, value2)

evalHiFun :: HiMonad m => HiFun -> [HiExpr] -> ExceptT HiError m HiValue
evalHiFun HiFunAdd args            = binary addValues args
evalHiFun HiFunSub args            = binary subValues args
evalHiFun HiFunMul args            = binary mulValues args
evalHiFun HiFunDiv args            = binary divValues args
evalHiFun HiFunNot args            = unary notValue args
evalHiFun HiFunAnd args            = lazyBinary andExpr args
evalHiFun HiFunOr args             = lazyBinary orExpr args
evalHiFun HiFunLessThan args       = binary (compareValues (<)) args
evalHiFun HiFunGreaterThan args    = binary (compareValues (>)) args
evalHiFun HiFunEquals args         = binary (compareValues (==)) args
evalHiFun HiFunNotLessThan args    = binary (compareValues (>=)) args
evalHiFun HiFunNotGreaterThan args = binary (compareValues (<=)) args
evalHiFun HiFunNotEquals args      = binary (compareValues (/=)) args
evalHiFun HiFunIf args             = ifFunction args
evalHiFun HiFunLength args         = unary lengthValue args
evalHiFun HiFunReverse args        = unary reverseValue args
evalHiFun HiFunToUpper args        = unary (textValue T.toUpper) args
evalHiFun HiFunToLower args        = unary (textValue T.toLower) args
evalHiFun HiFunTrim args           = unary (textValue T.strip) args
evalHiFun HiFunList args           = createList args
evalHiFun HiFunRange args          = binary rangeValues args
evalHiFun HiFunFold args           = binary foldValues args
evalHiFun HiFunPackBytes args      = unary packValue args
evalHiFun HiFunUnpackBytes args    = unary unpackValue args
evalHiFun HiFunEncodeUtf8 args     = unary encodeValue args
evalHiFun HiFunDecodeUtf8 args     = unary decodeValue args
evalHiFun HiFunZip args            = unary compressValue args
evalHiFun HiFunUnzip args          = unary decompressValue args
evalHiFun HiFunSerialise args      = unary serialiseValue args
evalHiFun HiFunDeserialise args    = unary deserialiseValue args
evalHiFun HiFunRead args           = unary readValue args
evalHiFun HiFunWrite args          = binary writeValue args
evalHiFun HiFunMkDir args          = unary mkDirValue args
evalHiFun HiFunChDir args          = unary cdValue args
evalHiFun HiFunParseTime args      = unary parseValue args
evalHiFun HiFunRand args           = binary randValue args
evalHiFun HiFunEcho args           = unary echoValue args
evalHiFun HiFunCount args          = unary countValue args
evalHiFun HiFunKeys args           = unary hiKeys args
evalHiFun HiFunValues args         = unary hiValues args
evalHiFun HiFunInvert args         = unary hiInvert args

unary :: HiMonad m => (HiValue -> ExceptT HiError m HiValue) -> [HiExpr] -> ExceptT HiError m HiValue
unary f [arg1] = do
  value <- evalHiExpr arg1
  f value
unary _ _ = throwError HiErrorArityMismatch

binary :: HiMonad m => (HiValue -> HiValue -> ExceptT HiError m HiValue) -> [HiExpr] -> ExceptT HiError m HiValue
binary f [arg1, arg2] = do
  value1 <- evalHiExpr arg1
  value2 <- evalHiExpr arg2
  f value1 value2
binary _ _ = throwError HiErrorArityMismatch

lazyBinary :: HiMonad m => (HiValue -> HiExpr -> ExceptT HiError m HiValue) -> [HiExpr] -> ExceptT HiError m HiValue
lazyBinary f [arg1, arg2] = do
  value <- evalHiExpr arg1
  f value arg2
lazyBinary _ _ = throwError HiErrorArityMismatch

hiValues :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiValues (HiValueDict dict) = return $ HiValueList $ S.fromList $ M.elems dict
hiValues _                  = throwError HiErrorInvalidArgument

hiKeys :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiKeys (HiValueDict dict) = return $ HiValueList $ S.fromList $ M.keys dict
hiKeys _                  = throwError HiErrorInvalidArgument

hiInvert :: HiMonad m => HiValue -> ExceptT HiError m HiValue
hiInvert (HiValueDict dict) = return $ HiValueDict $ M.map HiValueList (M.fromListWith (><) pairs)
  where
    pairs = [(val, S.singleton key) | (key, val) <- M.toList dict]
hiInvert _ = throwError HiErrorInvalidArgument

countValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
countValue (HiValueString text) = abstractCount (T.unpack text) (HiValueString . T.singleton)
countValue (HiValueBytes bytes) = abstractCount (map fromIntegral (B.unpack bytes)) HiValueNumber
countValue (HiValueList list)   = abstractCount (toList list) id
countValue _                    = throwError HiErrorInvalidArgument

abstractCount :: (HiMonad m, Ord a) => [a] -> (a -> HiValue) -> ExceptT HiError m HiValue
abstractCount collection keyF = do
  let ones = replicate (length collection) 1
  return $ HiValueDict $ M.mapKeys keyF $ M.map HiValueNumber $ M.fromListWith (+) (zip collection ones)

echoValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
echoValue (HiValueString text) = return $ HiValueAction $ HiActionEcho text
echoValue _                    = throwError HiErrorInvalidArgument

randValue :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
randValue (HiValueNumber number1) (HiValueNumber number2) = do
  int1 <- toInt number1
  int2 <- toInt number2
  return $ HiValueAction $ HiActionRand int1 int2
randValue _ _ = throwError HiErrorInvalidArgument

parseValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
parseValue (HiValueString text) = return $ maybe HiValueNull HiValueTime (readMaybe (T.unpack text))
parseValue _                    = throwError HiErrorInvalidArgument

cdValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
cdValue (HiValueString text) = return $ HiValueAction $ HiActionChDir $ T.unpack text
cdValue _                    = throwError HiErrorInvalidArgument

mkDirValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
mkDirValue (HiValueString text) = return $ HiValueAction $ HiActionMkDir $ T.unpack text
mkDirValue _                    = throwError HiErrorInvalidArgument

readValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
readValue (HiValueString text) = return $ HiValueAction $ HiActionRead $ T.unpack text
readValue _                    = throwError HiErrorInvalidArgument

writeValue :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
writeValue (HiValueString file) (HiValueString text) = return $ HiValueAction $ HiActionWrite (T.unpack file) (encodeUtf8 text)
writeValue (HiValueString file) (HiValueBytes bytes) = return $ HiValueAction $ HiActionWrite (T.unpack file) bytes
writeValue (HiValueString file) value = return $ HiValueAction $ HiActionWrite (T.unpack file) (toStrict $ serialise value)
writeValue _ _ = throwError HiErrorInvalidArgument

serialiseValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
serialiseValue value = return $ HiValueBytes $ toStrict $ serialise value

deserialiseValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
deserialiseValue (HiValueBytes bytes) = return $ deserialise $ fromStrict bytes
deserialiseValue _                    = throwError HiErrorInvalidArgument

compressValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
compressValue (HiValueBytes bytes) = return $ HiValueBytes $ toStrict $
  compressWith defaultCompressParams { compressLevel = bestCompression } $ fromStrict bytes
compressValue _ = throwError HiErrorInvalidArgument

decompressValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
decompressValue (HiValueBytes bytes) = return $ HiValueBytes $ toStrict $ decompress  $ fromStrict bytes
decompressValue _ = throwError HiErrorInvalidArgument

decodeValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
decodeValue (HiValueBytes bytes) = return $ case decodeUtf8' bytes of
  Left _     -> HiValueNull
  Right text -> HiValueString text
decodeValue _ = throwError HiErrorInvalidArgument

encodeValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
encodeValue (HiValueString text) = return $ HiValueBytes $ encodeUtf8 text
encodeValue _                    = throwError HiErrorInvalidArgument

packValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
packValue (HiValueList list) = HiValueBytes . B.pack . toList <$> mapM valueToByte list
packValue _                  = throwError HiErrorInvalidArgument

unpackValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
unpackValue (HiValueBytes bytes) = HiValueList . S.fromList <$> mapM bytesToValues (B.unpack bytes)
unpackValue _                    = throwError HiErrorInvalidArgument

bytesToValues :: HiMonad m => Word8 -> ExceptT HiError m HiValue
bytesToValues word = return $ HiValueNumber (fromIntegral word)

valueToByte :: HiMonad m => HiValue -> ExceptT HiError m Word8
valueToByte (HiValueNumber number) = do
  integer <- toInteger number
  if 0 <= integer && integer <= 255
    then return $ fromIntegral integer
    else throwError HiErrorInvalidArgument
valueToByte _ = throwError HiErrorInvalidArgument

reverseValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
reverseValue (HiValueString text) = return $ HiValueString $ T.reverse text
reverseValue (HiValueList list)   = return $ HiValueList $ S.reverse list
reverseValue _                    = throwError HiErrorInvalidArgument

foldValues :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
foldValues (HiValueFunction HiFunAdd) (HiValueList (x :<| xs)) = foldM addValues x xs
foldValues (HiValueFunction HiFunSub) (HiValueList (x :<| xs)) = foldM subValues x xs
foldValues (HiValueFunction HiFunMul) (HiValueList (x :<| xs)) = foldM mulValues x xs
foldValues (HiValueFunction HiFunDiv) (HiValueList (x :<| xs)) = foldM divValues x xs
foldValues (HiValueFunction HiFunLessThan) (HiValueList (x :<| xs)) = foldM (compareValues (<)) x xs
foldValues (HiValueFunction HiFunGreaterThan) (HiValueList (x :<| xs)) = foldM (compareValues (>)) x xs
foldValues (HiValueFunction HiFunEquals) (HiValueList (x :<| xs)) = foldM (compareValues (==)) x xs
foldValues (HiValueFunction HiFunNotLessThan) (HiValueList (x :<| xs)) = foldM (compareValues (>=)) x xs
foldValues (HiValueFunction HiFunNotGreaterThan) (HiValueList (x :<| xs)) = foldM (compareValues (<=)) x xs
foldValues (HiValueFunction HiFunNotEquals) (HiValueList (x :<| xs)) = foldM (compareValues (/=)) x xs
foldValues (HiValueFunction HiFunRange) (HiValueList (x :<| xs)) = foldM randValue x xs
foldValues (HiValueFunction HiFunFold) (HiValueList (x :<| xs)) = foldM foldValues x xs
foldValues (HiValueFunction HiFunWrite) (HiValueList (x :<| xs)) = foldM writeValue x xs
foldValues (HiValueFunction HiFunRand) (HiValueList (x :<| xs)) = foldM randValue x xs
foldValues (HiValueFunction HiFunAnd) (HiValueList (x :<| xs)) = foldM andExpr x (S.mapWithIndex (\_ val -> HiExprValue val) xs)
foldValues (HiValueFunction HiFunOr) (HiValueList (x :<| xs)) = foldM orExpr x (S.mapWithIndex (\_ val -> HiExprValue val) xs)
foldValues _ (HiValueList S.Empty) = return HiValueNull
foldValues _ _ = throwError HiErrorInvalidArgument

rangeValues :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
rangeValues (HiValueNumber left) (HiValueNumber right) =
  return $ HiValueList $ S.fromList $ map HiValueNumber [left .. right]
rangeValues _ _ = throwError HiErrorInvalidArgument

createList :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
createList args = HiValueList . S.fromList <$> mapM evalHiExpr args

evalSlice :: HiMonad m => HiValue -> [HiExpr] -> ExceptT HiError m HiValue
evalSlice (HiValueString text) [arg]        = unary (indexValue text) [arg]
evalSlice (HiValueList list) [arg]          = unary (indexValue list) [arg]
evalSlice (HiValueBytes bytes) [arg]        = unary (indexValue bytes) [arg]
evalSlice (HiValueDict dict) [arg]          = unary (getValue dict) [arg]
evalSlice (HiValueString text) [arg1, arg2] = binary (sliceValues text) [arg1, arg2]
evalSlice (HiValueList list) [arg1, arg2]   = binary (sliceValues list) [arg1, arg2]
evalSlice (HiValueBytes bytes) [arg1, arg2] = binary (sliceValues bytes) [arg1, arg2]
evalSlice _ _                               = throwError HiErrorArityMismatch

getValue :: HiMonad m => M.Map HiValue HiValue -> HiValue -> ExceptT HiError m HiValue
getValue dict key = return $ fromMaybe HiValueNull (M.lookup key dict)

indexValue :: (Sliceable a, HiMonad m) => a -> HiValue -> ExceptT HiError m HiValue
indexValue sliceable (HiValueNumber index) = do
  let size = len sliceable
  i <- toInt index
  if i < size && i >= 0
    then return $ at sliceable i
    else return HiValueNull
indexValue _ _ = throwError HiErrorInvalidArgument

sliceValues :: (Sliceable a, HiMonad m) => a -> HiValue -> HiValue -> ExceptT HiError m HiValue
sliceValues sliceable (HiValueNumber number1) (HiValueNumber number2) = do
  let size = len sliceable
  i <- transformIndex size number1
  j <- transformIndex size number2
  if i < j
    then return $ slice sliceable i j
    else return $ empty sliceable
sliceValues sliceable HiValueNull right@(HiValueNumber _) =
  sliceValues sliceable (HiValueNumber 0) right
sliceValues sliceable left@(HiValueNumber _) HiValueNull =
  sliceValues sliceable left (HiValueNumber $ fromIntegral $ len sliceable)
sliceValues sliceable HiValueNull HiValueNull =
  sliceValues sliceable (HiValueNumber 0) (HiValueNumber $ fromIntegral $ len sliceable)
sliceValues _ _ _ = throwError HiErrorInvalidArgument

transformIndex :: HiMonad m => Int -> Rational -> ExceptT HiError m Int
transformIndex size index = do
  ind <- toInt index
  let pos = if ind < 0 then size + ind else ind
  let transformed | pos < 0 = 0
                  | pos > size = size
                  | otherwise = pos
  return transformed

toInteger :: HiMonad m => Rational -> ExceptT HiError m Integer
toInteger rational = if denominator rational == 1 then return $ numerator rational else throwError HiErrorInvalidArgument

toInt :: HiMonad m => Rational -> ExceptT HiError m Int
toInt rational = do
  integer <- toInteger rational
  if fromIntegral (minBound :: Int) <= integer && integer <= fromIntegral (maxBound :: Int)
    then return $ fromIntegral integer
    else throwError HiErrorInvalidArgument

subValues :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
subValues (HiValueNumber number1) (HiValueNumber number2) = return $ HiValueNumber $ number1 - number2
subValues (HiValueTime time1) (HiValueTime time2) = return $ HiValueNumber $ realToFrac $ diffUTCTime time1 time2
subValues _ _ = throwError HiErrorInvalidArgument

divValues :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
divValues (HiValueString text1) (HiValueString text2) =
  return $ HiValueString $ T.dropWhileEnd (== '/') text1 <> T.pack "/" <> T.dropWhile (== '/') text2
divValues (HiValueNumber number1) (HiValueNumber number2) =
  if number2 == 0
    then throwError HiErrorDivideByZero
    else return $ HiValueNumber $ number1 / number2
divValues _ _ = throwError HiErrorInvalidArgument

addValues :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
addValues (HiValueString text1) (HiValueString text2) = return $ HiValueString $ text1 <> text2
addValues (HiValueList list1) (HiValueList list2) = return $ HiValueList $ list1 <> list2
addValues (HiValueBytes bytes1) (HiValueBytes bytes2) = return $ HiValueBytes $ bytes1 <> bytes2
addValues (HiValueTime time) (HiValueNumber number) = return $ HiValueTime $ addUTCTime (realToFrac number) time
addValues (HiValueNumber number1) (HiValueNumber number2) = return $ HiValueNumber $ number1 + number2
addValues _ _ = throwError HiErrorInvalidArgument

mulValues :: HiMonad m => HiValue -> HiValue -> ExceptT HiError m HiValue
mulValues (HiValueNumber number) (HiValueString text)     = reproduce number text HiValueString
mulValues (HiValueNumber number) (HiValueList list)       = reproduce number list HiValueList
mulValues (HiValueNumber number) (HiValueBytes bytes)     = reproduce number bytes HiValueBytes
mulValues (HiValueNumber number1) (HiValueNumber number2) = return $ HiValueNumber $ number1 * number2
mulValues val1 val2 =
  if val1 > val2
    then mulValues val2 val1
    else throwError HiErrorInvalidArgument

reproduce :: HiMonad m => (Semigroup list) => Rational -> list -> (list -> HiValue) -> ExceptT HiError m HiValue
reproduce number list constructor = do
  integer <- toInteger number
  if integer > 0
    then return $ constructor $ stimes integer list
    else throwError HiErrorInvalidArgument

lengthValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
lengthValue (HiValueString text) = return $ HiValueNumber $ toRational $ T.length text
lengthValue (HiValueList list)   = return $ HiValueNumber $ toRational $ S.length list
lengthValue _                    = throwError HiErrorInvalidArgument

textValue :: HiMonad m => (Text -> Text) -> HiValue -> ExceptT HiError m HiValue
textValue f (HiValueString text) = return $ HiValueString $ f text
textValue _ _                    = throwError HiErrorInvalidArgument

compareValues :: HiMonad m => (HiValue -> HiValue -> Bool) -> HiValue -> HiValue -> ExceptT HiError m HiValue
compareValues f arg1 arg2 = return $ HiValueBool $ f arg1 arg2

notValue :: HiMonad m => HiValue -> ExceptT HiError m HiValue
notValue (HiValueBool bool) = return $ HiValueBool $ not bool
notValue _                  = throwError HiErrorInvalidArgument

ifFunction :: HiMonad m => [HiExpr] -> ExceptT HiError m HiValue
ifFunction [arg1, arg2, arg3] = do
  first <- evalHiExpr arg1
  case first of
    HiValueBool bool ->
      if bool
        then evalHiExpr arg2
        else evalHiExpr arg3
    _ -> throwError HiErrorInvalidArgument
ifFunction _ = throwError HiErrorArityMismatch

andExpr :: HiMonad m => HiValue -> HiExpr -> ExceptT HiError m HiValue
andExpr value expr = if isFalseOrNull value then return value else evalHiExpr expr

orExpr :: HiMonad m => HiValue -> HiExpr -> ExceptT HiError m HiValue
orExpr value expr = if isFalseOrNull value then evalHiExpr expr else return value

isFalseOrNull :: HiValue -> Bool
isFalseOrNull HiValueNull        = True
isFalseOrNull (HiValueBool bool) = not bool
isFalseOrNull _                  = False
