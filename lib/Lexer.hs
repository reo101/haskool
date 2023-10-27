module Lexer (
  Token (..),
  lexer,
  intLexer,
  commentLexer,
  lineCommentLexer,
  unmatchedCommentLexer,
  eofLexer,
  symbolLexer,
  classLexer,
  ifLexer,
  thenLexer,
  elseLexer,
  fiLexer,
  inheritsLexer,
  isVoidLexer,
  letLexer,
  inLexer,
  whileLexer,
  loopLexer,
  poolLexer,
  caseLexer,
  esacLexer,
  newLexer,
  ofLexer,
  notLexer,
  trueLexer,
  falseLexer,
  typeIdLexer,
  objectIdLexer,
  darrowLexer,
  lessEqualLexer,
  whitespaceLexer,
  newLineLexer,
  stringLexer,
)
where

import Control.Applicative (Alternative (..), Applicative (liftA2))
import Control.Applicative.Combinators (manyTill_)
import Control.Lens (Field1 (_1), (%~), (&))
import Data.Attoparsec.Combinator (satisfyElem)
import Data.Attoparsec.Text (
  Parser,
  anyChar,
  char,
  choice,
  digit,
  endOfInput,
  manyTill,
  parseOnly,
  string,
 )
import Data.Bool.HT (if')
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, toLower, toUpper)
import Data.Either.Extra (fromRight', partitionEithers)
import Data.Monoid (Sum (..))
import Data.Text qualified as T
import Data.Tuple.Extra (both)
import Debug.Trace (traceShowId)

data Token
  = TComment Int
  | TEOF
  | TClass
  | TIf
  | TElse
  | TFi
  | TInherits
  | TIsVoid
  | TLet
  | TIn
  | TWhile
  | TLoop
  | TPool
  | TThen
  | TCase
  | TEsac
  | TNew
  | TOf
  | TNot
  | TBool Bool
  | TTypeId T.Text
  | TObjectId T.Text
  | TDArrow
  | TLessEqual
  | TAssign
  | TWhitespace
  | TNewLine
  | TSymbol Char -- [+/-*=<.~,;:()@{}]
  | TInvalid
  | TInteger T.Text
  | TString Int T.Text
  | TError Int T.Text
  deriving stock (Show, Eq)

lexer :: T.Text -> [Token]
lexer text =
  fromRight' $
    traceShowId $
      flip parseOnly text $
        flip manyTill eofLexer $
          -- many $
          choice
            [ intLexer
            , commentLexer
            , lineCommentLexer
            , unmatchedCommentLexer
            , classLexer
            , ifLexer
            , thenLexer
            , elseLexer
            , fiLexer
            , inheritsLexer
            , isVoidLexer
            , letLexer
            , inLexer
            , whileLexer
            , loopLexer
            , poolLexer
            , caseLexer
            , esacLexer
            , newLexer
            , ofLexer
            , notLexer
            , trueLexer
            , falseLexer
            , typeIdLexer
            , objectIdLexer
            , darrowLexer
            , lessEqualLexer
            , whitespaceLexer
            , newLineLexer
            , stringLexer
            , symbolLexer
            ]

intLexer :: Parser Token
intLexer = TInteger . T.pack <$> some digit

commentLexer :: Parser Token
commentLexer = (\(success, n) -> (if success then TComment else flip TError "EOF in comment") n) <$> go
 where
  go :: Parser (Bool, Int)
  go = do
    let commentStart = string "(*"
        commentEnd = string "*)"
        nestedCommentOrAny =
          choice
            [ (True, 1) <$ newLineLexer
            , go
            , (True, 0) <$ anyChar
            ]
    _ <- commentStart
    (linesSkipped, successfully) <-
      manyTill_
        nestedCommentOrAny
        (choice [True <$ commentEnd, False <$ eofLexer])
    let (badLines, goodLines) =
          both sum $
            partitionEithers $
              (\(success, n) -> if' success Right Left n)
                <$> linesSkipped
    if successfully
      then do
        pure (True, goodLines)
      else do
        pure (False, badLines + goodLines)

lineCommentLexer :: Parser Token
lineCommentLexer = do
  _ <- string "--"
  _ <- manyTill anyChar (choice [newLineLexer, eofLexer])
  pure $ TComment 1

unmatchedCommentLexer :: Parser Token
unmatchedCommentLexer = TError 0 "Unmatched *)" <$ string "*)"

eofLexer :: Parser Token
eofLexer = TEOF <$ endOfInput

symbolLexer :: Parser Token
symbolLexer = TSymbol <$> choice (char <$> "+/-*=<.~,;:()@{}")

stringCaseInsensitive :: String -> Parser String
stringCaseInsensitive = traverse (liftA2 (<|>) (char . toUpper) (char . toLower))

classLexer :: Parser Token
classLexer = TClass <$ stringCaseInsensitive "class"

ifLexer :: Parser Token
ifLexer = TIf <$ stringCaseInsensitive "if"

thenLexer :: Parser Token
thenLexer = TThen <$ stringCaseInsensitive "then"

elseLexer :: Parser Token
elseLexer = TElse <$ stringCaseInsensitive "else"

fiLexer :: Parser Token
fiLexer = TFi <$ stringCaseInsensitive "fi"

inheritsLexer :: Parser Token
inheritsLexer = TInherits <$ stringCaseInsensitive "inherits"

isVoidLexer :: Parser Token
isVoidLexer = TIsVoid <$ stringCaseInsensitive "isvoid"

letLexer :: Parser Token
letLexer = TLet <$ stringCaseInsensitive "let"

inLexer :: Parser Token
inLexer = TIn <$ stringCaseInsensitive "in"

whileLexer :: Parser Token
whileLexer = TWhile <$ stringCaseInsensitive "while"

loopLexer :: Parser Token
loopLexer = TLoop <$ stringCaseInsensitive "loop"

poolLexer :: Parser Token
poolLexer = TPool <$ stringCaseInsensitive "pool"

caseLexer :: Parser Token
caseLexer = TCase <$ stringCaseInsensitive "case"

esacLexer :: Parser Token
esacLexer = TEsac <$ stringCaseInsensitive "esac"

newLexer :: Parser Token
newLexer = TNew <$ stringCaseInsensitive "new"

ofLexer :: Parser Token
ofLexer = TOf <$ stringCaseInsensitive "of"

notLexer :: Parser Token
notLexer = TNot <$ stringCaseInsensitive "not"

trueLexer :: Parser Token
trueLexer = TBool True <$ stringCaseInsensitive "true"

falseLexer :: Parser Token
falseLexer = TBool False <$ stringCaseInsensitive "false"

isAlphaNumOrUnderscore :: Char -> Bool
isAlphaNumOrUnderscore = liftA2 (||) isAlphaNum (== '_')

typeIdLexer :: Parser Token
typeIdLexer = do
  first <- satisfyElem isAsciiUpper
  rest <- many $ satisfyElem isAlphaNumOrUnderscore
  pure $ TTypeId $ T.pack $ first : rest

objectIdLexer :: Parser Token
objectIdLexer = do
  first <- satisfyElem isAsciiLower
  rest <- many $ satisfyElem isAlphaNumOrUnderscore
  pure $ TObjectId $ T.pack $ first : rest

darrowLexer :: Parser Token
darrowLexer = TDArrow <$ string "=>"

lessEqualLexer :: Parser Token
lessEqualLexer = TLessEqual <$ string "<="

whitespaceLexer :: Parser Token
whitespaceLexer = TWhitespace <$ some (choice $ char <$> whitespaces)
 where
  whitespaces :: [Char]
  whitespaces =
    toEnum
      <$> [ 32 -- space
          , 12 -- formfeed
          , 13 -- carriage return
          , 9 -- tab
          , 11 -- vertical tab
          ]
newLineLexer :: Parser Token
newLineLexer = TNewLine <$ char '\n'

-- TODO: more character espaces

stringLexer :: Parser Token
stringLexer = do
  let quote = char '"'
  _ <- quote
  chrs <-
    manyTill
      (choice [escapedCharLexer, (1, '\n') <$ newLineLexer, (0,) <$> anyChar])
      quote
  -- Sum all `fst`s, leave `snd`s
  let (rows, str) =
        chrs
          & ((_1 %~ Sum) <$>)
          & sequenceA
          & (_1 %~ getSum)
  pure $ TString rows $ T.pack str
 where
  escapedCharLexer :: Parser (Int, Char)
  escapedCharLexer = do
    _ <- char '\\'
    ch <- anyChar
    pure $ case ch of
      'n' -> (1, '\n')
      '\n' -> (1, '\n')
      't' -> (0, '\t')
      _ -> (0, ch)
