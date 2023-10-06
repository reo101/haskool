{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant pure" #-}

module Regex.Atto (
  main,
) where

import Data.Attoparsec.Text (
  Parser,
  anyChar,
  char,
  choice,
  letter,
  many',
  option,
  parseOnly,
  (<?>),
 )
import Data.Text qualified as T
import Debug.Trace (traceShowM)
import Text.Printf (printf)
import Control.Applicative (many)

data Regex
  = Lit Char
  | AnyChar
  | Optional Regex
  | ZeroOrMore Regex
  | Concat [Regex]
  deriving stock (Show)

parseRegex :: Parser Regex
parseRegex = Concat <$> many' parseRegexSingle

parseRegexSingle :: Parser Regex
parseRegexSingle =
  choice
    [ parseOptional
    , parseZeroOrMore
    , parseAnyChar
    , parseLiteral
    ]

parseLiteral :: Parser Regex
parseLiteral = Lit <$> letter

parseAnyChar :: Parser Regex
parseAnyChar = AnyChar <$ char '.'

between :: Parser a -> Parser c -> Parser b -> Parser b
between left right real = left *> real <* right
-- between left right real = do
--   _ <- left
--   res <- real
--   _ <- right
--   pure res

afterSingletonOrBrackets :: (Regex -> Regex) -> Char -> Parser Regex
afterSingletonOrBrackets quantify ch = do
  res <- quantifiable
  _ <- char ch
  pure $ quantify res
 where
  quantifiable =
    choice
      [ between (char '(') (char ')') parseRegex
      , choice [parseLiteral, parseAnyChar]
      ]

parseOptional :: Parser Regex
parseOptional = afterSingletonOrBrackets Optional '?'

parseZeroOrMore :: Parser Regex
parseZeroOrMore = afterSingletonOrBrackets ZeroOrMore '*'

compileRegex :: Regex -> Parser T.Text
compileRegex (Lit ch) = T.singleton <$> char ch <?> printf "literal '%c'" ch
compileRegex AnyChar = T.singleton <$> anyChar <?> "any"
compileRegex (Optional r) = option "" (compileRegex r) <?> "optional"
compileRegex (ZeroOrMore r) = T.concat <$> many (compileRegex r) <?> "kleene"
compileRegex (Concat rs) = T.concat <$> traverse compileRegex rs <?> "concat"

main :: IO ()
main = do
  regexStr <- getLine
  str <- getLine
  let Right regex = parseOnly parseRegex $ T.pack regexStr
  let regexParser = compileRegex regex
  let result = parseOnly regexParser $ T.pack str
  putStrLn $ case result of
    Right matched -> printf "yes, %s, %d" (T.length matched) (T.length matched)
    Left err -> "no, " ++ err
