module Lexer where

import Data.Char (isAlpha, isAlphaNum, isDigit)
import Data.Text (Text)
import Data.Text qualified as T
import Text.Read.HT (maybeRead)

data Token
  = TClass
  | TType Text
  | TInherits
  | TLBrace
  | TRBrace
  | TID Text
  | TColon
  | TSemicolon
  | TComma
  | TArrow
  | TIf
  | TThen
  | TElse
  | TFi
  | TWhile
  | TLoop
  | TPool
  | TLet
  | TIn
  | TEnd
  | TCase
  | TOF
  | TEsac
  | TNew
  | TIsVoid
  | TPlus
  | TMinus
  | TMultiply
  | TDivide
  | TNegate
  | TLess
  | TLessEqual
  | TEqual
  | TNot
  | TOpenParen
  | TCloseParen
  | TDot
  | TSelf
  | TInteger Int
  | TString Text
  | TTrue
  | TFalse
  deriving stock (Show, Eq)

-- Lexer function
lexer :: Text -> [Token]
lexer input
  | T.null input = []
  | T.head input `elem` (" \t\n" :: String) = lexer (T.tail input)
  | T.isPrefixOf "class" input = TClass : lexer (T.drop 5 input)
  | T.isPrefixOf "inherits" input = TInherits : lexer (T.drop 8 input)
  | T.isPrefixOf "if" input = TIf : lexer (T.drop 2 input)
  | T.isPrefixOf "then" input = TThen : lexer (T.drop 4 input)
  | T.isPrefixOf "else" input = TElse : lexer (T.drop 4 input)
  | T.isPrefixOf "fi" input = TFi : lexer (T.drop 2 input)
  | T.isPrefixOf "while" input = TWhile : lexer (T.drop 5 input)
  | T.isPrefixOf "loop" input = TLoop : lexer (T.drop 4 input)
  | T.isPrefixOf "pool" input = TPool : lexer (T.drop 4 input)
  | T.isPrefixOf "let" input = TLet : lexer (T.drop 3 input)
  | T.isPrefixOf "in" input = TIn : lexer (T.drop 2 input)
  | T.isPrefixOf "end" input = TEnd : lexer (T.drop 3 input)
  | T.isPrefixOf "case" input = TCase : lexer (T.drop 4 input)
  | T.isPrefixOf "of" input = TOF : lexer (T.drop 2 input)
  | T.isPrefixOf "esac" input = TEsac : lexer (T.drop 4 input)
  | T.isPrefixOf "new" input = TNew : lexer (T.drop 3 input)
  | T.isPrefixOf "isvoid" input = TIsVoid : lexer (T.drop 6 input)
  | T.isPrefixOf "true" input = TTrue : lexer (T.drop 4 input)
  | T.isPrefixOf "false" input = TFalse : lexer (T.drop 5 input)
  | T.isPrefixOf "self." input = TSelf : TDot : lexer (T.drop 5 input)
  | T.isPrefixOf "=>" input = TArrow : lexer (T.drop 2 input)
  | T.isPrefixOf "." input = TDot : lexer (T.drop 1 input)
  | T.head input `elem` ("{}:;,+-*/~()=<>" :: String) = symbolToken : lexer (T.tail input)
  | T.head input == '"' = case T.break (== '"') (T.tail input) of
      (str, rest) -> TString str : lexer (T.drop 2 rest)
  | isAlpha (T.head input) = keywordOrIDToken : lexer (T.drop (T.length keywordOrIDText) input)
  | isDigit (T.head input) = case maybeRead (T.unpack numText) of
      Just num -> TInteger num : lexer (T.drop (T.length numText) input)
      Nothing -> error "Invalid number"
  | otherwise = error $ "Invalid character: " ++ [T.head input]
 where
  symbolToken = case T.head input of
    '{' -> TLBrace
    '}' -> TRBrace
    ':' -> TColon
    ';' -> TSemicolon
    ',' -> TComma
    '<' -> if T.head (T.tail input) == '-' then TArrow else TLess
    '+' -> TPlus
    '-' -> TMinus
    '*' -> TMultiply
    '/' -> TDivide
    '~' -> TNegate
    '(' -> TOpenParen
    ')' -> TCloseParen
    '=' -> if T.head (T.tail input) == '=' then TEqual else error "Invalid token: ="
    _ -> error $ "Invalid character: " ++ [T.head input]
  keywordOrIDText = T.takeWhile isAlphaNum input
  keywordOrIDToken = case keywordOrIDText of
    "class" -> TClass
    "inherits" -> TInherits
    "if" -> TIf
    "then" -> TThen
    "else" -> TElse
    "fi" -> TFi
    "while" -> TWhile
    "loop" -> TLoop
    "pool" -> TPool
    "let" -> TLet
    "in" -> TIn
    "end" -> TEnd
    "case" -> TCase
    "of" -> TOF
    "esac" -> TEsac
    "new" -> TNew
    "isvoid" -> TIsVoid
    "true" -> TTrue
    "false" -> TFalse
    _ -> TID keywordOrIDText
  numText = T.takeWhile isDigit input
