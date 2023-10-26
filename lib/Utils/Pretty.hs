module Utils.Pretty (
  lexAndPrettyPrint,
  prettyPrintToken,
) where

import Data.Text qualified as T
import Lexer (Token (..), lexer)
import Text.Printf (printf)
import System.FilePath.Lens (filename)
import Control.Lens ((^.))

lexAndPrettyPrint :: (FilePath, T.Text) -> T.Text
lexAndPrettyPrint (sourceFile, sourceCode) =
  let tokenss = lexer <$> T.lines sourceCode
      prettyLines :: [T.Text]
      prettyLines =
        zip @Int [1 ..] tokenss >>= \(line, tokens) -> do
          let numberedTokens = (line,) <$> tokens
          numberedTokenToPretty <$> numberedTokens
      header = T.pack $ printf
        "#name \"%s\""
        (sourceFile ^. filename)
   in T.unlines $ header : prettyLines
 where
  numberedTokenToPretty :: (Int, Token) -> T.Text
  numberedTokenToPretty (line, token) =
    T.pack $
      printf
        "#%s %s"
        (show line)
        (prettyPrintToken token)

prettyPrintToken :: Token -> String
prettyPrintToken = \case
  Kek -> "kek"
  TInteger num -> printf "INT_CONST %s" num
  _ -> "LAJNO"

-- prettyPrintToken :: Token -> String
-- prettyPrintToken = \case
--   TClass -> "TClass"
--   TType t -> "TType t"
--   TInherits -> "TInherits"
--   TLBrace -> "TLBrace"
--   TRBrace -> "TRBrace"
--   TID i -> printf "OBJECTID %s" i
--   TColon -> "TColon"
--   TSemicolon -> "TSemicolon"
--   TComma -> "TComma"
--   TArrow -> "TArrow"
--   TIf -> "TIf"
--   TThen -> "TThen"
--   TElse -> "TElse"
--   TFi -> "TFi"
--   TWhile -> "TWhile"
--   TLoop -> "TLoop"
--   TPool -> "TPool"
--   TLet -> "TLet"
--   TIn -> "TIn"
--   TEnd -> "TEnd"
--   TCase -> "TCase"
--   TOF -> "TOF"
--   TEsac -> "TEsac"
--   TNew -> "TNew"
--   TIsVoid -> "TIsVoid"
--   TPlus -> "TPlus"
--   TMinus -> "'-'"
--   TMultiply -> "'*'"
--   TDivide -> "'/'"
--   TNegate -> "TNegate"
--   TLess -> "TLess"
--   TLessEqual -> "TLessEqual"
--   TEqual -> "TEqual"
--   TNot -> "TNot"
--   TOpenParen -> "TOpenParen"
--   TCloseParen -> "TCloseParen"
--   TDot -> "TDot"
--   TSelf -> "TSelf"
--   TInteger i -> printf "INT_CONST %s" i
--   TString s -> "TString s"
--   TTrue -> "TTrue"
--   TFalse -> "TFalse"
--   TError e -> "TError e"
