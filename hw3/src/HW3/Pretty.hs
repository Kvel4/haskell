module HW3.Pretty
  ( prettyValue
  ) where

import Data.ByteString (ByteString, unpack)
import Data.Foldable (toList)
import Data.Map (Map)
import qualified Data.Map as D
import Data.Scientific (floatingOrInteger, fromRationalRepetendUnlimited)
import Data.Sequence (Seq)
import Data.Word (Word8)
import GHC.Real (Ratio ((:%)))
import HW3.Base (HiAction (..), HiValue (..))
import Numeric (showFFloat, showHex)
import Prettyprinter hiding (list, prettyList, sep)
import Prettyprinter.Render.Terminal (AnsiStyle)

prettyValue :: HiValue -> Doc AnsiStyle
prettyValue (HiValueNumber number) = prettyNumber number
prettyValue (HiValueBool bool)     = prettyBool bool
prettyValue (HiValueFunction fun)  = pretty $ show fun
prettyValue (HiValueString text)   = viaShow text
prettyValue (HiValueList list)     = prettyList list
prettyValue (HiValueBytes bytes)   = prettyBytes bytes
prettyValue (HiValueAction action) = prettyAction action
prettyValue (HiValueTime time)     = unaryInvocation "parse-time" ("\"" ++ show time ++ "\"")
prettyValue (HiValueDict dict)     = prettyDict dict
prettyValue HiValueNull            = pretty "null"

prettyAction :: HiAction -> Doc AnsiStyle
prettyAction (HiActionRead path)        = unaryInvocation "read" (show path)
prettyAction (HiActionWrite path bytes) = binaryInvocation "write" (show path) (show $ prettyBytes bytes)
prettyAction (HiActionMkDir path)       = unaryInvocation "mkdir" (show path)
prettyAction (HiActionChDir path)       = unaryInvocation "cd" (show path)
prettyAction (HiActionRand int1 int2)   = binaryInvocation "rand" int1 int2
prettyAction (HiActionEcho text)        = unaryInvocation "echo" (show text)
prettyAction HiActionCwd                = pretty "cwd"
prettyAction HiActionNow                = pretty "now"

unaryInvocation :: String -> String -> Doc AnsiStyle
unaryInvocation name arg = pretty name <> parens (pretty arg)

binaryInvocation :: (Pretty a1, Pretty a2) => String -> a1 -> a2 -> Doc AnsiStyle
binaryInvocation name arg1 arg2 = pretty name <> parens (pretty arg1 <> comma <+> pretty arg2)

prettyCollection :: Doc AnsiStyle -> Doc AnsiStyle -> String -> (a -> Doc AnsiStyle) -> [a] -> Doc AnsiStyle
prettyCollection left right _ _ [] = left <+> right
prettyCollection left right sep f collection =
 group $ encloseSep (left <> space) (space <> right) (pretty sep) (map f collection)

prettyDict :: Map HiValue HiValue -> Doc AnsiStyle
prettyDict dict = prettyCollection lbrace rbrace ", " prettyEntry (D.toList dict)

prettyEntry :: (HiValue, HiValue) -> Doc AnsiStyle
prettyEntry (key, val) = prettyValue key <> colon <+> prettyValue val

prettyBytes :: ByteString -> Doc AnsiStyle
prettyBytes bytes = prettyCollection (pretty "[#") (pretty "#]") " " prettyByte (unpack bytes)

prettyByte :: Word8 -> Doc AnsiStyle
prettyByte byte = if byte < 16 then pretty "0" <> pretty (showHex byte "") else pretty (showHex byte "")

prettyList :: Seq HiValue -> Doc AnsiStyle
prettyList list = prettyCollection lbracket rbracket ", " prettyValue (toList list)

prettyBool :: Bool -> Doc AnsiStyle
prettyBool bool = pretty $ if bool then "true" else "false"

prettyNumber :: Rational -> Doc AnsiStyle
prettyNumber rational@(dividend :% divider) = do
  let (finite, mbRepetendInd) = fromRationalRepetendUnlimited rational
  let (qt, rm) = quotRem dividend divider
  case mbRepetendInd of
    Just _ -> do
      -- infinite
      if qt == 0
        then pretty rm <> slash <> pretty divider --fraction
        else pretty qt <+> sign rm <+> pretty (abs rm) <> slash <> pretty divider --mixed fraction
    Nothing ->
      -- finite
      case floatingOrInteger finite :: (Either Double Integer) of
        Left double -> pretty $ showFFloat Nothing double ""
        Right int   -> pretty int
  where
    sign int = pretty $ if signum int == 1 then "+" else "-"
