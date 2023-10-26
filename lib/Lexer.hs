module Lexer (
  Token (..),
  lexer,
)
where

import Control.Applicative (Alternative (..))
import Data.Attoparsec.Text (
  Parser,
  choice,
  digit,
  parseOnly, manyTill, anyChar, string,
 )
import Data.Either.Extra (fromRight')
import Data.Text qualified as T
import Control.Monad (void)

data Token
  = Kek
  | TComment
  | TInteger T.Text

lexer :: T.Text -> [Token]
lexer text =
  fromRight' $
    flip parseOnly text $
      choice
        [ intLexer
        ]

intLexer :: Parser [Token]
intLexer = pure . TInteger . T.pack <$> many digit

commentLexer :: Parser ()
commentLexer = do
  let commentStart = string "(*"
      commentEnd = string "*)"
      nestedCommentOrAny = choice [commentLexer, void anyChar]
  _ <- commentStart
  _ <- manyTill nestedCommentOrAny commentEnd
  pure ()

-- >>> parseOnly commentLexer "(* ((((((( (*  )))) ( kek *) )))(((()))))))))) asdkf *)"
-- Right ()

