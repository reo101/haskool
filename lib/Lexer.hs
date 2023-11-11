module Lexer (
  Lexeme (..),
  lexer,
  intLexer,
  commentLexer,
  lineCommentLexer,
  unmatchedCommentLexer,
  eofLexer,
  operatorLexer,
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
import Control.Lens (Field1 (_1), Field2 (_2), (%~), (&), (^?), _Just)
import Control.Monad (join)
import Data.Attoparsec.Combinator (lookAhead, satisfyElem)
import Data.Attoparsec.Text (
  Parser,
  anyChar,
  char,
  choice,
  endOfInput,
  manyTill,
  parseOnly,
  string,
 )
import Data.Bool.HT (if')
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, toLower, toUpper, isDigit)
import Data.Either.Extra (fromRight', partitionEithers)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Text qualified as T
import Data.Tuple.Extra (both)
import Data.Function (on)

data Lexeme
  = LComment Int
  | LEOF
  | LClass
  | LIf
  | LElse
  | LFi
  | LInherits
  | LIsVoid
  | LLet
  | LIn
  | LWhile
  | LLoop
  | LPool
  | LThen
  | LCase
  | LEsac
  | LNew
  | LOf
  | LNot
  | LBool Bool
  | LTypeId T.Text
  | LObjectId T.Text
  | LDArrow
  | LLessEqual
  | LAssign
  | LWhitespace
  | LNewLine
  | LSymbol Char -- [+/-*=<.~,;:()@{}]
  | LInteger Int
  | LString Int T.Text
  | LError Int Int T.Text
  deriving stock (Show, Ord, Eq)

lexer :: T.Text -> [Lexeme]
lexer text =
  fromRight' $
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
          , operatorLexer
          , invalidCharLexer
          ]

-- FIXME: parse or no parse?
intLexer :: Parser Lexeme
intLexer = LInteger . digitsToNumber <$> some digit
 where
  digit :: Parser Int
  digit = do
    digitChar <- satisfyElem isDigit
    pure $ ((-) `on` fromEnum) digitChar '0'

  digitsToNumber :: [Int] -> Int
  digitsToNumber = foldl (\acc x -> acc * 10 + x) 0


commentLexer :: Parser Lexeme
commentLexer = (\(success, n) -> (if success then LComment else flip (join LError) "EOF in comment") n) <$> go
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
    -- pure (successfully, badLines + goodLines)
    if successfully
      then do
      -- NOTE: `badLines` should be 0
        pure (True, goodLines)
      else do
        pure (False, badLines + goodLines)

lineCommentLexer :: Parser Lexeme
lineCommentLexer = do
  _ <- string "--"
  _ <- manyTill anyChar (choice [newLineLexer, eofLexer])
  pure $ LComment 1

unmatchedCommentLexer :: Parser Lexeme
unmatchedCommentLexer = LError 0 0 "Unmatched *)" <$ string "*)"

eofLexer :: Parser Lexeme
eofLexer = LEOF <$ endOfInput

operatorLexer :: Parser Lexeme
operatorLexer = do
  choice
    [ LLessEqual <$ string "<="
    , LDArrow <$ string "=>"
    , LAssign <$ string "<-"
    , LSymbol <$> choice (char <$> ";{}(,):@.+-*/~<=")
    ]

keywordCaseInsensitive :: String -> Parser String
keywordCaseInsensitive str = do
  let insensitiveChar :: Char -> Parser Char
      insensitiveChar = liftA2 (<|>) (char . toUpper) (char . toLower)
  insStr <- traverse insensitiveChar str
  -- NOTE: might need to remove `Num` from `isAlphaNumOrUnderscore`
  _ <- lookAhead (satisfyElem $ not . isAlphaNumOrUnderscore)
  pure insStr

classLexer :: Parser Lexeme
classLexer = LClass <$ keywordCaseInsensitive "class"

ifLexer :: Parser Lexeme
ifLexer = LIf <$ keywordCaseInsensitive "if"

thenLexer :: Parser Lexeme
thenLexer = LThen <$ keywordCaseInsensitive "then"

elseLexer :: Parser Lexeme
elseLexer = LElse <$ keywordCaseInsensitive "else"

fiLexer :: Parser Lexeme
fiLexer = LFi <$ keywordCaseInsensitive "fi"

inheritsLexer :: Parser Lexeme
inheritsLexer = LInherits <$ keywordCaseInsensitive "inherits"

isVoidLexer :: Parser Lexeme
isVoidLexer = LIsVoid <$ keywordCaseInsensitive "isvoid"

letLexer :: Parser Lexeme
letLexer = LLet <$ keywordCaseInsensitive "let"

inLexer :: Parser Lexeme
inLexer = LIn <$ keywordCaseInsensitive "in"

whileLexer :: Parser Lexeme
whileLexer = LWhile <$ keywordCaseInsensitive "while"

loopLexer :: Parser Lexeme
loopLexer = LLoop <$ keywordCaseInsensitive "loop"

poolLexer :: Parser Lexeme
poolLexer = LPool <$ keywordCaseInsensitive "pool"

caseLexer :: Parser Lexeme
caseLexer = LCase <$ keywordCaseInsensitive "case"

esacLexer :: Parser Lexeme
esacLexer = LEsac <$ keywordCaseInsensitive "esac"

newLexer :: Parser Lexeme
newLexer = LNew <$ keywordCaseInsensitive "new"

ofLexer :: Parser Lexeme
ofLexer = LOf <$ keywordCaseInsensitive "of"

notLexer :: Parser Lexeme
notLexer = LNot <$ keywordCaseInsensitive "not"

trueLexer :: Parser Lexeme
trueLexer = LBool True <$ (char 't' *> keywordCaseInsensitive "rue")

falseLexer :: Parser Lexeme
falseLexer = LBool False <$ (char 'f' *> keywordCaseInsensitive "alse")

isAlphaNumOrUnderscore :: Char -> Bool
isAlphaNumOrUnderscore = liftA2 (||) isAlphaNum (== '_')

typeIdLexer :: Parser Lexeme
typeIdLexer = do
  first <- satisfyElem isAsciiUpper
  rest <- many $ satisfyElem isAlphaNumOrUnderscore
  pure $ LTypeId $ T.pack $ first : rest

objectIdLexer :: Parser Lexeme
objectIdLexer = do
  first <- satisfyElem isAsciiLower
  rest <- many $ satisfyElem isAlphaNumOrUnderscore
  pure $ LObjectId $ T.pack $ first : rest

darrowLexer :: Parser Lexeme
darrowLexer = LDArrow <$ string "=>"

lessEqualLexer :: Parser Lexeme
lessEqualLexer = LLessEqual <$ string "<="

whitespaceLexer :: Parser Lexeme
whitespaceLexer = LWhitespace <$ some (choice $ char <$> whitespaces)
 where
  whitespaces :: [Char]
  whitespaces =
    toEnum
      <$> [ 32 -- space
          , 12 -- formfeed
          , 13 -- carriage return
          , 09 -- tab
          , 11 -- vertical tab
          ]

newLineLexer :: Parser Lexeme
newLineLexer = LNewLine <$ char '\n'

stringLexer :: Parser Lexeme
stringLexer = do
  let quote = char '"'
  _ <- quote
  (chrs, maybeErrorMessage) <-
    manyTill_
      (choice [escapedCharLexer, (1, "\n") <$ newLineLexer, (0,) . pure <$> anyChar])
      ( choice
          [ Nothing <$ quote
          , Just (0, "EOF in string constant") <$ eofLexer
          , Just (1, "Unterminated string constant") <$ newLineLexer
          ]
      )
  -- Sum all `fst`s, leave `snd`s
  let (lines, str) =
        chrs
          & ((_1 %~ Sum) <$>)
          & sequenceA
          & (_1 %~ getSum)
          & (_2 %~ concat)

  let skippedLines = fromMaybe 0 $ maybeErrorMessage ^? _Just . _1

  if
      -- HACK: ONLY (`null` / `escaped null`) + `unterminated` results
      --       in an error whose `display line` and `consumed lines` differ
      | "\\\0" `isInfixOf` str -> do
          pure $ LError lines (lines + skippedLines) "String contains escaped null character."
      | '\0' `elem` str -> do
          pure $ LError lines (lines + skippedLines) "String contains null character."
      | Just (_, errorMessage) <- maybeErrorMessage -> do
          pure $ join LError (lines + skippedLines) errorMessage
      | length str > 1024 -> do
          pure $ join LError lines "String constant too long"
      | otherwise -> do
          pure $ LString lines $ T.pack str
 where
  escapedCharLexer :: Parser (Int, String)
  escapedCharLexer = do
    _ <- char '\\'
    ch <- anyChar
    pure $ case ch of
      '\n' -> (1, "\n")
      'n' -> (0, "\n")
      't' -> (0, "\t")
      'f' -> (0, "\f")
      'b' -> (0, "\b")
      -- HACK: escaped null character is a different error
      --       than a plain null character
      '\0' -> (0, "\\\0")
      _ -> (0, [ch])

invalidCharLexer :: Parser Lexeme
invalidCharLexer = join LError 0 . T.pack . pure <$> anyChar
