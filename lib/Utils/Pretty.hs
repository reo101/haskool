module Utils.Pretty (
  lexAndPrettyPrint,
  prettyPrintToken,
) where

import Control.Lens (Field1 (_1), Field2 (_2), (%=), (^.), (+=))
import Control.Monad.State (MonadState (..), State, evalState)
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Lexer (Token (..), lexer)
import System.FilePath.Lens (filename)
import Text.Printf (printf)

lexAndPrettyPrint :: (FilePath, T.Text) -> T.Text
lexAndPrettyPrint (sourceFile, sourceCode) =
  let
    tokens :: [Token]
    tokens = lexer sourceCode
    linenize :: State (Int, [Token]) [T.Text]
    linenize = do
      (line, tokens) <- get
      -- TODO: clean up
      case listToMaybe tokens of
        Nothing -> pure []
        Just token -> do
          case token of
            TNewLine -> do
              _1 %= succ
              _2 %= tail
              linenize
            TComment n -> do
              _1 += n
              _2 %= tail
              linenize
            TWhitespace -> do
              _2 %= tail
              linenize
            TString n _ -> do
              _1 += n
              _2 %= tail
              (numberedTokenToPretty (line + n, token) :) <$> linenize
            TError n _ -> do
              _1 += n
              _2 %= tail
              (numberedTokenToPretty (line + n, token) :) <$> linenize
            _ -> do
              _2 %= tail
              (numberedTokenToPretty (line, token) :) <$> linenize
    prettyLines :: [T.Text]
    prettyLines = evalState linenize (1, tokens)
    header :: T.Text
    header =
      T.pack $
        printf
          "#name \"%s\""
          (sourceFile ^. filename)
   in
    T.unlines $ header : prettyLines
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
  TInteger i -> printf "INT_CONST %s" i
  TString _ s -> printf "STR_CONST %s" (show s)
  -- TComment n -> printf "NZ %s" ("TComment" :: String)
  -- TEOF -> printf "NZ %s" ("TEOF" :: String)
  TClass -> printf "CLASS"
  TIf -> printf "IF"
  TThen -> printf "THEN"
  TElse -> printf "ELSE"
  TFi -> printf "FI"
  TInherits -> printf "INHERITS"
  TIsVoid -> printf "ISVOID"
  TLet -> printf "LET"
  TIn -> printf "IN"
  TWhile -> printf "WHILE"
  TLoop -> printf "LOOP"
  TPool -> printf "POOL"
  TCase -> printf "CASE"
  TEsac -> printf "ESAC"
  TNew -> printf "NEW"
  TOf -> printf "OF"
  TNot -> printf "NOT"
  TBool p -> printf "BOOL_CONST %s" (T.pack $ toLower <$> show p)
  TTypeId i -> printf "TYPEID %s" i
  TObjectId i -> printf "OBJECTID %s" i
  TDArrow -> printf "DArrow"
  TLessEqual -> printf "NZ %s" ("TLessEqual" :: String)
  TAssign -> printf "ASSIGN"
  TWhitespace -> printf "NZ %s" ("TWhitespace" :: String)
  -- TNewLine -> printf "NZ %s" ("TNewLine" :: String)
  TSymbol ch -> printf "'%s'" [ch]
  TInvalid -> printf "NZ %s" ("TInvalid" :: String)
  TError _ e -> printf "ERROR %s" (show e)
  _ -> "### SHOULD NOT BE PRETTY PRINTED ###"
