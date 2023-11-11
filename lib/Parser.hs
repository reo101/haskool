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
import Lexer (Lexeme (..))
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
  parent <- optional do
    _ <- single LInherits
    _ <- whitespace
    typeId <- typeIdLexemeParser
    _ <- whitespace
    pure typeId
  _ <- single (LSymbol '{')
  _ <- try $ optional whitespace
  features <- many $ featureParser <* semicolonSeparator
  _ <- try $ optional whitespace
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
    _ <- optional whitespace
    _ <- colonSeparator
    _ <- optional whitespace
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
        mtype <- optional do
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
        , functionCallParser
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
  bbody <- optional do
    _ <- lexemeSeparator LAssign
    expressionParser
  pure $ SBinding{bidentifier, btype, bbody}

assignmentParser :: Parser SExpr
assignmentParser = do
  aid <- objectIdLexemeParser
  _ <- whitespace
  _ <- single LAssign
  abody <- expressionParser
  pure $ SEAssignment{aid, abody}

functionCallParser :: Parser SExpr
functionCallParser = do
  fcallee <- objectIdLexemeParser
  farguments <- betweenParenthesis argumentsParser
  pure $ SEFunctionCall{fcallee, farguments}

ifThenElseParser :: Parser SExpr
ifThenElseParser = do
  _ <- single LIf
  iif <- expressionParser
  _ <- single LThen
  ithen <- expressionParser
  _ <- single LElse
  ielse <- expressionParser
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
  bexpressions <- expressionParser `NE.endBy1` try semicolonSeparator
  _ <- single $ LSymbol '}'
  pure $ SEBlock{bexpressions}

letBindingParser :: Parser SExpr
letBindingParser = do
  _ <- single LLet
  lbindings <- bindingParser `NE.sepBy1` try commaSeparator
  _ <- single LIn
  lbody <- expressionParser
  pure $ SELetIn{lbindings, lbody}

caseParser :: Parser SExpr
caseParser = do
  _ <- single LCase
  cexpr <- expressionParser
  _ <- single LOf
  cprongs <- caseProngParser `NE.sepBy1` try semicolonSeparator
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

---

someProgram :: T.Text
someProgram = "class Main inherits \nKek { kek : Banica; fibonacci() : Kek { mqu + 1.banica(2) }; };\n\n\n\n\n\n"

-- >>> parse programParser "kek" $ lexer someProgram
-- Right (SProgram (SClass {name = "Main", parent = Just "Kek", features = [SFeatureMember (SBinding {bidentifier = "kek", btype = "Banica", bbody = Nothing}),SFeatureMethod {fidentifier = "fibonacci", fformals = [], ftype = "Kek", fbody = SEPlus {pleft = SEIdentifier {iid = "mqu"}, pright = SEMethodCall {mcallee = SEInteger {iint = 1}, mtype = Nothing, mname = "banica", marguments = [SEInteger {iint = 2}]}}}]} :| []))

-- >>> parse featureParser "kek" $ lexer "fibonacci() : Kek { mqu }"
-- Right (SFeatureMethod {fidentifier = "fibonacci", fformals = [], ftype = "Kek", fbody = SEIdentifier {iid = "mqu"}})

-- >>> parse expressionParser "123" $ lexer "a.b(1)+c(2,3).d().e(\"kek\")"
-- Right (SEPlus {pleft = SEMethodCall {mcallee = SEIdentifier {iid = "a"}, mtype = Nothing, mname = "b", marguments = [SEInteger {iint = 1}]}, pright = SEMethodCall {mcallee = SEMethodCall {mcallee = SEFunctionCall {fcallee = "c", farguments = [SEInteger {iint = 2},SEInteger {iint = 3}]}, mtype = Nothing, mname = "d", marguments = []}, mtype = Nothing, mname = "e", marguments = [SEString {sstring = "kek"}]}})

-- >>> parse expressionParser "123" $ lexer "not not true"
-- Right (SENot {nexpr = SENot {nexpr = SEIdentifier {iid = "true"}}})

-- >>> parse expressionParser "123" $ lexer "a(   1, 2   , 3    )"
-- Right (SEFunctionCall {fcallee = "a", farguments = [SEInteger {iint = 1},SEInteger {iint = 2},SEInteger {iint = 3}]})
