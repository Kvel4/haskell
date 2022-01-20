{-# LANGUAGE DeriveFunctor #-}

module HW3.Action
  ( HIO (..)
  , HiPermission (..)
  , PermissionException (..)
  ) where

import Control.Exception.Base (Exception, throwIO)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Sequence as S
import Data.Set (Set, member)
import qualified Data.Text as T
import Data.Time.Clock.System (getSystemTime, systemToUTCTime)
import HW3.Base (HiAction (..), HiMonad (..), HiValue (..))
import System.Directory (doesFileExist, listDirectory, createDirectory, setCurrentDirectory,
                         getCurrentDirectory)
import System.Random.Stateful (randomRIO)
import Data.Text.Encoding (decodeUtf8')

data HiPermission
  = AllowRead
  | AllowWrite
  | AllowTime
  deriving (Show, Eq, Ord, Enum, Bounded)

newtype PermissionException = PermissionRequired HiPermission deriving Show

instance Exception PermissionException

newtype HIO a = HIO { runHIO :: Set HiPermission -> IO a } deriving (Functor)

instance Applicative HIO where
  pure a = HIO $ \_ -> pure a

  (<*>) mf ma = HIO $ \perm -> do
     f <- runHIO mf perm
     a <- runHIO ma perm
     pure $ f a

instance Monad HIO where
  (>>=) ma f = HIO $ \perm -> do
    a <- runHIO ma perm
    runHIO (f a) perm

instance HiMonad HIO where
  runAction (HiActionRead path) = require AllowRead $ do
    isFile <- doesFileExist path
    if isFile
      then do
        bytes <- BS.toStrict <$> BS.readFile path
        case decodeUtf8' bytes of
          Left _     -> return $ HiValueBytes bytes
          Right text -> return $ HiValueString text
      else HiValueList . S.fromList . fmap (HiValueString . T.pack) <$> listDirectory path
  runAction (HiActionWrite path bytes) = require AllowWrite $ HiValueNull <$ BS.writeFile path (BS.fromStrict bytes)
  runAction (HiActionMkDir path) = require AllowWrite $ HiValueNull <$ createDirectory path
  runAction (HiActionChDir path) = require AllowRead $ HiValueNull <$ setCurrentDirectory path
  runAction HiActionCwd = require AllowRead $ HiValueString . T.pack <$> getCurrentDirectory
  runAction HiActionNow = require AllowTime $ HiValueTime . systemToUTCTime <$> getSystemTime
  runAction (HiActionRand int1 int2) = HIO $ \_ -> HiValueNumber . fromIntegral <$> randomRIO (int1, int2)
  runAction (HiActionEcho text) = require AllowWrite $ fmap (const HiValueNull) (putStrLn $ T.unpack text)

require :: HiPermission -> IO HiValue -> HIO HiValue
require perm func = HIO $ \perms ->
  if member perm perms
    then func
    else throwIO $ PermissionRequired perm
