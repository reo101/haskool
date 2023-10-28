module Lexer (
  Token (..),
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
  digit,
  endOfInput,
  manyTill,
  parseOnly,
  string,
 )
import Data.Bool.HT (if')
import Data.Char (isAlphaNum, isAsciiLower, isAsciiUpper, toLower, toUpper)
import Data.Either.Extra (fromRight', partitionEithers)
import Data.List (isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum (..))
import Data.Text qualified as T
import Data.Tuple.Extra (both)

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
  | TInteger T.Text
  | TString Int T.Text
  | TError Int Int T.Text
  deriving stock (Show, Eq)

lexer :: T.Text -> [Token]
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

intLexer :: Parser Token
intLexer = TInteger . T.pack <$> some digit

commentLexer :: Parser Token
commentLexer = (\(success, n) -> (if success then TComment else flip (join TError) "EOF in comment") n) <$> go
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

lineCommentLexer :: Parser Token
lineCommentLexer = do
  _ <- string "--"
  _ <- manyTill anyChar (choice [newLineLexer, eofLexer])
  pure $ TComment 1

unmatchedCommentLexer :: Parser Token
unmatchedCommentLexer = TError 0 0 "Unmatched *)" <$ string "*)"

eofLexer :: Parser Token
eofLexer = TEOF <$ endOfInput

operatorLexer :: Parser Token
operatorLexer = do
  choice
    [ TLessEqual <$ string "<="
    , TDArrow <$ string "=>"
    , TAssign <$ string "<-"
    , TSymbol <$> choice (char <$> ";{}(,):@.+-*/~<=")
    ]

keywordCaseInsensitive :: String -> Parser String
keywordCaseInsensitive str = do
  let insensitiveChar :: Char -> Parser Char
      insensitiveChar = liftA2 (<|>) (char . toUpper) (char . toLower)
  insStr <- traverse insensitiveChar str
  -- NOTE: might need to remove `Num` from `isAlphaNumOrUnderscore`
  _ <- lookAhead (satisfyElem $ not . isAlphaNumOrUnderscore)
  pure insStr

classLexer :: Parser Token
classLexer = TClass <$ keywordCaseInsensitive "class"

ifLexer :: Parser Token
ifLexer = TIf <$ keywordCaseInsensitive "if"

thenLexer :: Parser Token
thenLexer = TThen <$ keywordCaseInsensitive "then"

elseLexer :: Parser Token
elseLexer = TElse <$ keywordCaseInsensitive "else"

fiLexer :: Parser Token
fiLexer = TFi <$ keywordCaseInsensitive "fi"

inheritsLexer :: Parser Token
inheritsLexer = TInherits <$ keywordCaseInsensitive "inherits"

isVoidLexer :: Parser Token
isVoidLexer = TIsVoid <$ keywordCaseInsensitive "isvoid"

letLexer :: Parser Token
letLexer = TLet <$ keywordCaseInsensitive "let"

inLexer :: Parser Token
inLexer = TIn <$ keywordCaseInsensitive "in"

whileLexer :: Parser Token
whileLexer = TWhile <$ keywordCaseInsensitive "while"

loopLexer :: Parser Token
loopLexer = TLoop <$ keywordCaseInsensitive "loop"

poolLexer :: Parser Token
poolLexer = TPool <$ keywordCaseInsensitive "pool"

caseLexer :: Parser Token
caseLexer = TCase <$ keywordCaseInsensitive "case"

esacLexer :: Parser Token
esacLexer = TEsac <$ keywordCaseInsensitive "esac"

newLexer :: Parser Token
newLexer = TNew <$ keywordCaseInsensitive "new"

ofLexer :: Parser Token
ofLexer = TOf <$ keywordCaseInsensitive "of"

notLexer :: Parser Token
notLexer = TNot <$ keywordCaseInsensitive "not"

trueLexer :: Parser Token
trueLexer = TBool True <$ (char 't' *> keywordCaseInsensitive "rue")

falseLexer :: Parser Token
falseLexer = TBool False <$ (char 'f' *> keywordCaseInsensitive "alse")

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
          , 09 -- tab
          , 11 -- vertical tab
          ]

newLineLexer :: Parser Token
newLineLexer = TNewLine <$ char '\n'

-- TODO: more character espaces

stringLexer :: Parser Token
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
          pure $ TError lines (lines + skippedLines) "String contains escaped null character."
      | '\0' `elem` str -> do
          pure $ TError lines (lines + skippedLines) "String contains null character."
      | Just (_, errorMessage) <- maybeErrorMessage -> do
          pure $ join TError (lines + skippedLines) errorMessage
      | length str > 1024 -> do
          pure $ join TError lines "String constant too long"
      | otherwise -> do
          pure $ TString lines $ T.pack str
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

invalidCharLexer :: Parser Token
invalidCharLexer = join TError 0 . T.pack . pure <$> anyChar
