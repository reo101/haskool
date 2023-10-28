module Utils.Pretty (
  lexAndPrettyPrint,
  prettyPrintToken,
) where

import Control.Lens (Field1 (_1), Field2 (_2), (%=), (+=), (^.))
import Control.Monad.State (MonadState (..), State, evalState)
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Lexer (Token (..), lexer)
import Numeric (showOct)
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
            TError showLine skipLines _ -> do
              _1 += skipLines
              _2 %= tail
              (numberedTokenToPretty (line + showLine, token) :) <$> linenize
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

charToMaybeOctal :: Char -> String
charToMaybeOctal ch =
  if
      | fromEnum ch
          `elem` escapedCharacters ->
          printf "\\%03s" (showOct (fromEnum ch) "")
      | ch `elem` ['\\', '"'] -> printf "\\%s" [ch]
      | otherwise -> case ch of
          '\n' -> printf "\\n"
          '\t' -> printf "\\t"
          '\b' -> printf "\\b"
          '\f' -> printf "\\f"
          _ -> printf "%s" [ch]
 where
  escapedCharacters =
    [ 0o00
    , 0o01
    , 0o02
    , 0o03
    , 0o04
    , 0o13
    , 0o15
    , 0o22
    , 0o33
    ]

prettyPrintToken :: Token -> String
prettyPrintToken = \case
  TInteger i -> printf "INT_CONST %s" i
  TString _ s -> printf "STR_CONST \"%s\"" (T.unpack s >>= charToMaybeOctal)
  -- TComment n -> undefined
  -- TEOF -> undefined
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
  TDArrow -> printf "DARROW"
  TLessEqual -> printf "LE"
  TAssign -> printf "ASSIGN"
  -- TWhitespace -> undefined
  -- TNewLine -> undefined
  TSymbol ch -> printf "'%s'" [ch]
  TError _ _ e -> printf "ERROR \"%s\"" (T.unpack e >>= charToMaybeOctal)
  _ -> "### SHOULD NOT BE PRETTY PRINTED ###"
