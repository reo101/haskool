module Utils.Pretty (
  wrapAndIntersperse,
  lexAndPrettyPrint,
  lexParseAndPrettyPrint,
  prettyPrintLexeme,
) where

import Control.Lens (Field1 (_1), Field2 (_2), use, (%=), (+=), (^.))
import Control.Monad.State (State, evalState)
import Data.Char (toLower)
import Data.List (intercalate, intersperse)
import Data.Maybe (listToMaybe)
import Data.Text qualified as T
import Lexer (Lexeme (..), lexer)
import Numeric (showOct)
import System.FilePath.Lens (filename)
import Text.Printf (printf)

wrapAndIntersperse :: String -> String -> String -> [String] -> String
wrapAndIntersperse left middle right xs = left ++ intercalate middle xs ++ right

lexParseAndPrettyPrint :: (FilePath, T.Text) -> T.Text
lexParseAndPrettyPrint (sourceFile, sourceCode) = undefined

lexAndPrettyPrint :: (FilePath, T.Text) -> T.Text
lexAndPrettyPrint (sourceFile, sourceCode) =
  T.unlines $ header : prettyLines
 where
  tokens :: [Lexeme]
  tokens = lexer sourceCode

  prettyLines :: [T.Text]
  prettyLines = evalState linenize (1, tokens)

  header :: T.Text
  header =
    T.pack $
      printf
        "#name \"%s\""
        (sourceFile ^. filename)

linenize :: State (Int, [Lexeme]) [T.Text]
linenize = do
  maybeLexeme <- takeLexeme
  case maybeLexeme of
    Just token -> do
      line <- use _1
      process <- processLexeme line token
      maybe id (:) process <$> linenize
    Nothing -> do
      pure []
 where
  takeLexeme :: State (Int, [Lexeme]) (Maybe Lexeme)
  takeLexeme = do
    tokens <- use _2
    case listToMaybe tokens of
      Nothing -> do
        pure Nothing
      Just token -> do
        _2 %= tail
        pure $ Just token

numberedLexemeToPretty :: (Int, Lexeme) -> T.Text
numberedLexemeToPretty (line, token) =
  T.pack $
    printf
      "#%s %s"
      (show line)
      (prettyPrintLexeme token)

processLexeme :: Int -> Lexeme -> State (Int, [Lexeme]) (Maybe T.Text)
processLexeme line token = do
  _1 += skipLines
  pure maybePrintedLexeme
 where
  (skipLines, maybePrintedLexeme) = case token of
    LNewLine ->
      (1, Nothing)
    LComment n ->
      (n, Nothing)
    LWhitespace ->
      (0, Nothing)
    LString n _ ->
      (n, Just $ numberedLexemeToPretty (line + n, token))
    LError showLine skipLines _ ->
      (skipLines, Just $ numberedLexemeToPretty (line + showLine, token))
    _ ->
      (0, Just $ numberedLexemeToPretty (line, token))

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

prettyPrintLexeme :: Lexeme -> String
prettyPrintLexeme = \case
  LInteger i -> printf "INT_CONST %s" (show i)
  LString _ s -> printf "STR_CONST \"%s\"" (T.unpack s >>= charToMaybeOctal)
  -- LComment n -> undefined
  -- LEOF -> undefined
  LClass -> printf "CLASS"
  LIf -> printf "IF"
  LThen -> printf "THEN"
  LElse -> printf "ELSE"
  LFi -> printf "FI"
  LInherits -> printf "INHERITS"
  LIsVoid -> printf "ISVOID"
  LLet -> printf "LET"
  LIn -> printf "IN"
  LWhile -> printf "WHILE"
  LLoop -> printf "LOOP"
  LPool -> printf "POOL"
  LCase -> printf "CASE"
  LEsac -> printf "ESAC"
  LNew -> printf "NEW"
  LOf -> printf "OF"
  LNot -> printf "NOT"
  LBool p -> printf "BOOL_CONST %s" (T.pack $ toLower <$> show p)
  LTypeId i -> printf "TYPEID %s" i
  LObjectId i -> printf "OBJECTID %s" i
  LDArrow -> printf "DARROW"
  LLessEqual -> printf "LE"
  LAssign -> printf "ASSIGN"
  -- LWhitespace -> undefined
  -- LNewLine -> undefined
  LSymbol ch -> printf "'%s'" [ch]
  LError _ _ e -> printf "ERROR \"%s\"" (T.unpack e >>= charToMaybeOctal)
  _ -> "### SHOULD NOT BE PRETTY PRINTED ###"
