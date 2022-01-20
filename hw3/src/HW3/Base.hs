{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE DerivingStrategies  #-}
{-# LANGUAGE FlexibleInstances   #-}

module HW3.Base
  ( HiAction (..)
  , HiError (..)
  , HiExpr (..)
  , HiFun (..)
  , HiMonad (..)
  , HiValue (..)
  , Sliceable (..)
  ) where

import Codec.Serialise.Class (Serialise)
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.Map (Map)
import Data.Ratio (Rational)
import Data.Sequence (Seq)
import qualified Data.Sequence as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime)
import GHC.Generics (Generic)

data HiFun
  = HiFunDiv
  | HiFunMul
  | HiFunAdd
  | HiFunSub
  | HiFunNot
  | HiFunAnd
  | HiFunOr
  | HiFunLessThan
  | HiFunGreaterThan
  | HiFunEquals
  | HiFunNotLessThan
  | HiFunNotGreaterThan
  | HiFunNotEquals
  | HiFunIf
  | HiFunLength
  | HiFunToUpper
  | HiFunToLower
  | HiFunReverse
  | HiFunTrim
  | HiFunList
  | HiFunRange
  | HiFunFold
  | HiFunPackBytes
  | HiFunUnpackBytes
  | HiFunEncodeUtf8
  | HiFunDecodeUtf8
  | HiFunZip
  | HiFunUnzip
  | HiFunSerialise
  | HiFunDeserialise
  | HiFunRead
  | HiFunWrite
  | HiFunMkDir
  | HiFunChDir
  | HiFunParseTime
  | HiFunRand
  | HiFunEcho
  | HiFunCount
  | HiFunKeys
  | HiFunValues
  | HiFunInvert
  deriving (Eq, Ord, Generic, Serialise)

data HiValue
  = HiValueFunction HiFun
  | HiValueBool Bool
  | HiValueNumber Data.Ratio.Rational
  | HiValueNull
  | HiValueString Text
  | HiValueList (Seq HiValue)
  | HiValueBytes ByteString
  | HiValueAction HiAction
  | HiValueTime UTCTime
  | HiValueDict (Map HiValue HiValue)
  deriving (Show, Eq, Ord, Generic, Serialise)

data HiExpr
  = HiExprValue HiValue
  | HiExprApply HiExpr [HiExpr]
  | HiExprRun HiExpr
  | HiExprDict [(HiExpr, HiExpr)]
  deriving  (Show, Eq, Ord, Generic, Serialise)

data HiError
  = HiErrorInvalidArgument
  | HiErrorInvalidFunction
  | HiErrorArityMismatch
  | HiErrorDivideByZero
  deriving (Show, Eq, Ord, Generic, Serialise)

data HiAction
  = HiActionRead  FilePath
  | HiActionWrite FilePath ByteString
  | HiActionMkDir FilePath
  | HiActionChDir FilePath
  | HiActionCwd
  | HiActionNow
  | HiActionRand Int Int
  | HiActionEcho Text

  deriving (Show, Eq, Ord, Generic, Serialise)

class Monad m => HiMonad m where
  runAction :: HiAction -> m HiValue

class Sliceable a where
  at :: a -> Int -> HiValue
  slice :: a -> Int -> Int -> HiValue
  empty :: a -> HiValue
  len :: a -> Int

instance Sliceable ByteString where
  at bytes = HiValueNumber . fromIntegral . B.index bytes
  slice bytes i j = (HiValueBytes . B.take (j - i) . B.drop i) bytes
  empty _ = HiValueBytes mempty
  len = B.length

instance Sliceable (Seq HiValue) where
  at list = S.index list
  slice list i j = (HiValueList . S.take (j - i) . S.drop i) list
  empty _ = HiValueList mempty
  len = S.length

instance Sliceable Text where
  at text = HiValueString . T.singleton . T.index text
  slice text i j = (HiValueString . T.take (j - i) . T.drop i) text
  empty _ = HiValueString mempty
  len = T.length

instance Show HiFun where
  show HiFunMul            = "mul"
  show HiFunDiv            = "div"
  show HiFunAdd            = "add"
  show HiFunSub            = "sub"
  show HiFunAnd            = "and"
  show HiFunOr             = "or"
  show HiFunLessThan       = "less-than"
  show HiFunGreaterThan    = "greater-than"
  show HiFunEquals         = "equals"
  show HiFunNotLessThan    = "not-less-than"
  show HiFunNotGreaterThan = "not-greater-than"
  show HiFunNotEquals      = "not-equals"
  show HiFunNot            = "not"
  show HiFunIf             = "if"
  show HiFunLength         = "length"
  show HiFunToUpper        = "to-upper"
  show HiFunToLower        = "to-lower"
  show HiFunReverse        = "reverse"
  show HiFunTrim           = "trim"
  show HiFunList           = "list"
  show HiFunRange          = "range"
  show HiFunFold           = "fold"
  show HiFunPackBytes      = "pack-bytes"
  show HiFunUnpackBytes    = "unpack-bytes"
  show HiFunEncodeUtf8     = "encode-utf8"
  show HiFunDecodeUtf8     = "decode-utf8"
  show HiFunZip            = "zip"
  show HiFunUnzip          = "unzip"
  show HiFunSerialise      = "serialise"
  show HiFunDeserialise    = "deserialise"
  show HiFunRead           = "read"
  show HiFunWrite          = "write"
  show HiFunMkDir          = "mkdir"
  show HiFunChDir          = "cd"
  show HiFunParseTime      = "parse-time"
  show HiFunRand           = "rand"
  show HiFunEcho           = "echo"
  show HiFunCount          = "count"
  show HiFunKeys           = "keys"
  show HiFunValues         = "values"
  show HiFunInvert         = "invert"
