module Utils.Pretty (
  lexAndPrettyPrint,
  prettyPrintToken,
) where

import Control.Lens (Field1 (_1), Field2 (_2), use, (%=), (+=), (^.))
import Control.Monad.State (State, evalState)
import Data.Char (toLower)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Lexer (Token (..), lexer)
import Numeric (showOct)
import System.FilePath.Lens (filename)
import Text.Printf (printf)

lexAndPrettyPrint :: (FilePath, T.Text) -> T.Text
lexAndPrettyPrint (sourceFile, sourceCode) =
  T.unlines $ header : prettyLines
 where
  tokens :: [Token]
  tokens = lexer sourceCode

  prettyLines :: [T.Text]
  prettyLines = evalState linenize (1, tokens)

  header :: T.Text
  header =
    T.pack $
      printf
        "#name \"%s\""
        (sourceFile ^. filename)

linenize :: State (Int, [Token]) [T.Text]
linenize = do
  maybeToken <- takeToken
  case maybeToken of
    Just token -> do
      line <- use _1
      process <- processToken line token
      maybe id (:) process <$> linenize
    Nothing -> do
      pure []
 where
  takeToken :: State (Int, [Token]) (Maybe Token)
  takeToken = do
    tokens <- use _2
    case listToMaybe tokens of
      Nothing -> do
        pure Nothing
      Just token -> do
        _2 %= tail
        pure $ Just token

numberedTokenToPretty :: (Int, Token) -> T.Text
numberedTokenToPretty (line, token) =
  T.pack $
    printf
      "#%s %s"
      (show line)
      (prettyPrintToken token)

processToken :: Int -> Token -> State (Int, [Token]) (Maybe T.Text)
processToken line token = do
  _1 += skipLines
  pure maybePrintedToken
 where
  (skipLines, maybePrintedToken) = case token of
    TNewLine ->
      (1, Nothing)
    TComment n ->
      (n, Nothing)
    TWhitespace ->
      (0, Nothing)
    TString n _ ->
      (n, Just $ numberedTokenToPretty (line + n, token))
    TError showLine skipLines _ ->
      (skipLines, Just $ numberedTokenToPretty (line + showLine, token))
    _ ->
      (0, Just $ numberedTokenToPretty (line, token))

charToMaybeOctal :: Char -> String
charToMaybeOctal ch =
  if
      | fromEnum ch `elem` escapedCharacters ->
          printf "\\%03s" (showOct (fromEnum ch) "")
      | ch `elem` ['\\', '"'] ->
          printf "\\%s" [ch]
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
