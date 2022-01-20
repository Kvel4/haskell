module Main
  ( main
  ) where

import Control.Monad.Cont (lift)
import Data.Set (fromList)
import HW3.Action
import HW3.Evaluator
import HW3.Parser
import HW3.Pretty
import System.Console.Haskeline (InputT, defaultSettings, getInputLine, outputStrLn, runInputT)


main :: IO ()
main = runInputT defaultSettings loop
  where
    loop :: InputT IO ()
    loop = do
      minput <- getInputLine "hi> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just input -> do
          case parse input of
            Left err -> outputStrLn $ show err
            Right expr -> do
              ex <- lift $ runHIO (eval expr) (fromList [AllowRead, AllowWrite, AllowTime])
              case ex of
                Left err    -> outputStrLn $ show err
                Right value -> outputStrLn $ show $ prettyValue value
          loop
