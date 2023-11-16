{-# LANGUAGE DuplicateRecordFields #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# HLINT ignore "Redundant pure" #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Parser (
  parser,
) where

import Control.Applicative (Alternative (..))
import Control.Applicative.Combinators qualified (choice)
import Control.Monad (join, void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Combinators.NonEmpty qualified as NE (endBy1, sepBy1)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Void (Void)
import Lexer (Lexeme (..), lexer)
import Parser.Types (
  SBinding (..),
  SCaseProng (..),
  SClass (..),
  SExpr (..),
  SFeature (..),
  SFormal (..),
  SProgram (..),
 )
import Text.Megaparsec (
  MonadParsec (token, try),
  Parsec,
  between,
  optional,
  parse,
  satisfy,
  sepBy,
  single,
 )

type Parser = Parsec Void [Lexeme]

choice :: [Parser a] -> Parser a
choice = Control.Applicative.Combinators.choice . (try <$>)

---

parser :: Parser SProgram
parser = programParser

programParser :: Parser SProgram
programParser = SProgram <$> classParser `NE.endBy1` try semicolonSeparator

objectIdLexemeParser :: Parser T.Text
objectIdLexemeParser = do
  token isId Set.empty
 where
  isId :: Lexeme -> Maybe T.Text
  isId (LObjectId x) = Just x
  isId _ = Nothing

typeIdLexemeParser :: Parser T.Text
typeIdLexemeParser = do
  token isTypeId Set.empty
 where
  isTypeId :: Lexeme -> Maybe T.Text
  isTypeId (LTypeId x) = Just x
  isTypeId _ = Nothing

integerLexemeParser :: Parser Integer
integerLexemeParser = do
  token isInteger Set.empty
 where
  isInteger :: Lexeme -> Maybe Integer
  isInteger (LInteger x) = Just $ fromIntegral x
  isInteger _ = Nothing

stringLexemeParser :: Parser T.Text
stringLexemeParser = do
  token isString Set.empty
 where
  isString :: Lexeme -> Maybe T.Text
  isString (LString _ x) = Just x
  isString _ = Nothing

boolLexemeParser :: Parser Bool
boolLexemeParser = do
  token isBool Set.empty
 where
  isBool :: Lexeme -> Maybe Bool
  isBool (LBool x) = Just x
  isBool _ = Nothing

classParser :: Parser SClass
classParser = do
  _ <- single LClass
  _ <- whitespace
  name <- typeIdLexemeParser
  _ <- whitespace
  parent <- optional $ try do
    _ <- single LInherits
    _ <- whitespace
    typeId <- typeIdLexemeParser
    _ <- whitespace
    pure typeId
  _ <- single (LSymbol '{')
  _ <- optional whitespace
  features <- many $ featureParser <* semicolonSeparator
  _ <- optional whitespace
  _ <- single (LSymbol '}')
  pure $ SClass{name, parent, features}

whitespace :: Parser ()
whitespace = void $ many $ satisfy (`elem` [LWhitespace, LNewLine])

argumentsParser :: Parser [SExpr]
argumentsParser = expressionParser `sepBy` try commaSeparator

lexemeSeparator :: Lexeme -> Parser ()
lexemeSeparator = separatorParser . single

commaSeparator :: Parser ()
commaSeparator = lexemeSeparator $ LSymbol ','

colonSeparator :: Parser ()
colonSeparator = lexemeSeparator $ LSymbol ':'

semicolonSeparator :: Parser ()
semicolonSeparator = lexemeSeparator $ LSymbol ';'

separatorParser :: Parser a -> Parser ()
separatorParser = void . join between (optional whitespace)

-- separatorParser sep = do
--   _ <- optional whitespace
--   _ <- sep
--   _ <- optional whitespace
--   pure ()

betweenParenthesis :: Parser a -> Parser a
betweenParenthesis = between (single (LSymbol '(') *> optional whitespace) (optional whitespace <* single (LSymbol ')'))

formalParser :: Parser SFormal
formalParser = do
  fidentifier <- objectIdLexemeParser
  ftype <- typeIdLexemeParser
  pure $ SFormal{fidentifier, ftype}

featureParser :: Parser SFeature
featureParser = choice [featureMethodParser, featureMemberParser]
 where
  featureMethodParser :: Parser SFeature
  featureMethodParser = do
    fidentifier <- objectIdLexemeParser
    _ <- optional whitespace
    fformals <- betweenParenthesis $ formalParser `sepBy` try commaSeparator
    _ <- colonSeparator
    ftype <- typeIdLexemeParser
    _ <- optional whitespace
    _ <- single $ LSymbol '{'
    _ <- optional whitespace
    fbody <- expressionParser
    _ <- optional whitespace
    _ <- single $ LSymbol '}'
    pure $ SFeatureMethod{fidentifier, fformals, ftype, fbody}
  featureMemberParser :: Parser SFeature
  featureMemberParser = SFeatureMember <$> bindingParser

operators :: [[Operator Parser SExpr]]
operators =
  [
    [ Postfix methodCallSuffixParser
    ]
  ,
    [ InfixL (SETimes <$ try (lexemeSeparator (LSymbol '*')))
    , InfixL (SEDivide <$ try (lexemeSeparator (LSymbol '/')))
    ]
  ,
    [ InfixL (SEPlus <$ try (lexemeSeparator (LSymbol '+')))
    , InfixL (SEMinus <$ try (lexemeSeparator (LSymbol '-')))
    ]
  ,
    [ Prefix notParser
    , Prefix isvoidParser
    ]
  ,
    [ InfixN (SEEquals <$ try (lexemeSeparator (LSymbol '=')))
    , InfixN (SELt <$ try (lexemeSeparator (LSymbol '<')))
    , InfixN (SELte <$ try (lexemeSeparator LLessEqual))
    ]
  ]
 where
  recursiveUnaryOperatorParser :: Parser a -> (a -> SExpr -> SExpr) -> Parser (SExpr -> SExpr)
  recursiveUnaryOperatorParser p c = do
    a <- p
    maybeRec <- optional $ recursiveUnaryOperatorParser p c
    pure $
      fromMaybe id maybeRec . c a

  methodCallSuffixParser :: Parser (SExpr -> SExpr)
  methodCallSuffixParser =
    recursiveUnaryOperatorParser
      do
        mtype <- optional $ try do
          _ <- single $ LSymbol '@'
          typeIdLexemeParser
        _ <- single $ LSymbol '.'
        mname <- objectIdLexemeParser
        marguments <- betweenParenthesis $ expressionParser `sepBy` try commaSeparator
        pure (mtype, mname, marguments)
      (\(mtype, mname, marguments) mcallee -> SEMethodCall{mcallee, mtype, mname, marguments})

  notParser :: Parser (SExpr -> SExpr)
  notParser =
    recursiveUnaryOperatorParser
      (lexemeSeparator LNot)
      (const SENot)

  isvoidParser :: Parser (SExpr -> SExpr)
  isvoidParser =
    recursiveUnaryOperatorParser
      (lexemeSeparator LIsVoid)
      (const SEIsVoid)

expressionParser :: Parser SExpr
expressionParser = do
  base <-
    flip makeExprParser operators $
      choice
        [ assignmentParser
        , selfMethodCallParser
        , ifThenElseParser
        , whileParser
        , blockParser
        , letBindingParser
        , caseParser
        , newParser
        , tildeParser
        , bracketedParser
        , identifierParser
        , integerParser
        , stringParser
        , boolParser
        ]

  pure base

bindingParser :: Parser SBinding
bindingParser = do
  bidentifier <- objectIdLexemeParser
  _ <- colonSeparator
  btype <- typeIdLexemeParser
  bbody <- optional $ try do
    _ <- optional whitespace
    _ <- lexemeSeparator LAssign
    _ <- optional whitespace
    expressionParser
  pure $ SBinding{bidentifier, btype, bbody}

assignmentParser :: Parser SExpr
assignmentParser = do
  aid <- objectIdLexemeParser
  _ <- optional whitespace
  _ <- single LAssign
  _ <- optional whitespace
  abody <- expressionParser
  pure $ SEAssignment{aid, abody}

selfMethodCallParser :: Parser SExpr
selfMethodCallParser = do
  mname <- objectIdLexemeParser
  -- TODO: can it be optional?
  _ <- optional whitespace
  marguments <- betweenParenthesis argumentsParser
  pure $
    SEMethodCall
      { mcallee = SEIdentifier{iid = "self"}
      , mtype = Nothing
      , mname
      , marguments
      }

ifThenElseParser :: Parser SExpr
ifThenElseParser = do
  _ <- single LIf
  _ <- whitespace
  iif <- expressionParser
  _ <- whitespace
  _ <- single LThen
  _ <- whitespace
  ithen <- expressionParser
  _ <- whitespace
  _ <- single LElse
  _ <- whitespace
  ielse <- expressionParser
  _ <- whitespace
  _ <- single LFi
  pure $ SEIfThenElse{iif, ithen, ielse}

whileParser :: Parser SExpr
whileParser = do
  _ <- single LWhile
  wif <- expressionParser
  _ <- single LLoop
  wloop <- expressionParser
  _ <- single LPool
  pure $ SEWhile{wif, wloop}

blockParser :: Parser SExpr
blockParser = do
  _ <- single $ LSymbol '{'
  _ <- optional whitespace
  bexpressions <- expressionParser `NE.endBy1` try semicolonSeparator
  _ <- optional whitespace
  _ <- single $ LSymbol '}'
  pure $ SEBlock{bexpressions}

letBindingParser :: Parser SExpr
letBindingParser = do
  _ <- single LLet
  _ <- optional whitespace
  lbindings <- bindingParser `NE.sepBy1` try commaSeparator
  _ <- optional whitespace
  _ <- single LIn
  _ <- optional whitespace
  lbody <- expressionParser
  pure $ SELetIn{lbindings, lbody}

caseParser :: Parser SExpr
caseParser = do
  _ <- single LCase
  _ <- optional whitespace
  cexpr <- expressionParser
  _ <- optional whitespace
  _ <- single LOf
  _ <- optional whitespace
  cprongs <- caseProngParser `NE.sepBy1` try semicolonSeparator
  _ <- optional whitespace
  _ <- single LEsac
  pure $ SECase{cexpr, cprongs}
 where
  caseProngParser :: Parser SCaseProng
  caseProngParser = do
    pidenifier <- objectIdLexemeParser
    _ <- colonSeparator
    ptype <- typeIdLexemeParser
    _ <- separatorParser $ single LDArrow
    pbody <- expressionParser
    pure $ SCaseProng{pidenifier, ptype, pbody}

newParser :: Parser SExpr
newParser = do
  _ <- single LNew
  _ <- whitespace
  ntype <- typeIdLexemeParser
  pure $ SENew{ntype}

tildeParser :: Parser SExpr
tildeParser = do
  _ <- single $ LSymbol '~'
  _ <- whitespace
  texpr <- expressionParser
  pure $ SETilde{texpr}

bracketedParser :: Parser SExpr
bracketedParser = do
  bexpr <- betweenParenthesis expressionParser
  pure $ SEBracketed{bexpr}

identifierParser :: Parser SExpr
identifierParser = do
  iid <- objectIdLexemeParser
  pure $ SEIdentifier{iid}

integerParser :: Parser SExpr
integerParser = do
  iint <- integerLexemeParser
  pure $ SEInteger{iint}

stringParser :: Parser SExpr
stringParser = do
  sstring <- stringLexemeParser
  pure $ SEString{sstring}

boolParser :: Parser SExpr
boolParser = do
  bbool <- boolLexemeParser
  pure $ SEBool{bbool}
