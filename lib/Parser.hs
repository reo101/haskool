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
import Control.Lens (Iso', iso, lens, view, (+~))
import Control.Lens.Combinators (Lens')
import Control.Monad (join, void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Combinators.NonEmpty qualified as NE (endBy1, sepBy1)
import Data.List.NonEmpty as NE (last)
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
  MonadParsec (getParserState, token, try, updateParserState),
  Parsec,
  Pos,
  PosState (..),
  SourcePos (..),
  State (statePosState),
  between,
  mkPos,
  optional,
  parse,
  sepBy,
  single,
  unPos,
 )
import qualified Text.Megaparsec.Error

type Parser = Parsec Void [Lexeme]

choice :: [Parser a] -> Parser a
choice = Control.Applicative.Combinators.choice . (try <$>)

---

parser :: Parser SProgram
parser = programParser

programParser :: Parser SProgram
programParser = do
  pclasses <- classParser `NE.endBy1` try semicolonSeparator
  let endLine = (NE.last pclasses).endLine
  pure $ SProgram{endLine, pclasses}

objectIdLexemeParser :: Parser T.Text
objectIdLexemeParser = token isId Set.empty
 where
  isId :: Lexeme -> Maybe T.Text
  isId (LObjectId x) = Just x
  isId _ = Nothing

typeIdLexemeParser :: Parser T.Text
typeIdLexemeParser = token isTypeId Set.empty
 where
  isTypeId :: Lexeme -> Maybe T.Text
  isTypeId (LTypeId x) = Just x
  isTypeId _ = Nothing

integerLexemeParser :: Parser Integer
integerLexemeParser = token isInteger Set.empty
 where
  isInteger :: Lexeme -> Maybe Integer
  isInteger (LInteger x) = Just $ fromIntegral x
  isInteger _ = Nothing

stringLexemeParser :: Parser T.Text
stringLexemeParser = do
  (n, x) <- token isString Set.empty
  updateParserState $ lineNumberLens +~ n
  pure x
 where
  isString :: Lexeme -> Maybe (Int, T.Text)
  isString (LString n x) = Just (n, x)
  isString _ = Nothing

boolLexemeParser :: Parser Bool
boolLexemeParser = token isBool Set.empty
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
  endLine <- view lineNumberLens <$> getParserState
  pure $ SClass{endLine, name, parent, features}

whitespace :: Parser ()
whitespace = do
  skippedLines <-
    (sum <$>) $
      many $
        token
          ( \case
              LComment n -> Just n
              LNewLine -> Just 1
              LWhitespace -> Just 0
              _ -> Nothing
          )
          Set.empty
  updateParserState $ lineNumberLens +~ skippedLines
  pure ()

statePosState' :: Lens' (State s e) (PosState s)
statePosState' = lens statePosState (\s p -> s{statePosState = p})

pstateSourcePos' :: Lens' (PosState s) SourcePos
pstateSourcePos' = lens pstateSourcePos (\s p -> s{pstateSourcePos = p})

sourceLine' :: Lens' SourcePos Pos
sourceLine' = lens sourceLine (\s p -> s{sourceLine = p})

pos' :: Iso' Pos Int
pos' = iso unPos mkPos

lineNumberLens :: Lens' (State s e) Int
lineNumberLens = statePosState' . pstateSourcePos' . sourceLine' . pos'

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
  endLine <- view lineNumberLens <$> getParserState
  pure $ SFormal{endLine, fidentifier, ftype}

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
    endLine <- view lineNumberLens <$> getParserState
    pure $ SFeatureMethod{endLine, fidentifier, fformals, ftype, fbody}
  featureMemberParser :: Parser SFeature
  featureMemberParser = do
    fbinding <- bindingParser
    endLine <- view lineNumberLens <$> getParserState
    pure $ SFeatureMember{endLine, fbinding}

operators :: [[Operator Parser SExpr]]
operators =
  [
    [ Postfix methodCallSuffixParser
    ]
  ,
    [ Prefix isvoidParser
    , Prefix tildeParser
    ]
  ,
    [ InfixL (infixOperatorFromLexeme (LSymbol '*') SETimes)
    , InfixL (infixOperatorFromLexeme (LSymbol '/') SEDivide)
    ]
  ,
    [ InfixL (infixOperatorFromLexeme (LSymbol '+') SEPlus)
    , InfixL (infixOperatorFromLexeme (LSymbol '-') SEMinus)
    ]
  ,
    [ InfixN (infixOperatorFromLexeme (LSymbol '=') SEEquals)
    , InfixN (infixOperatorFromLexeme (LSymbol '<') SELt)
    , InfixN (infixOperatorFromLexeme LLessEqual SELte)
    ]
  ,
    [ Prefix notParser
    ]
  ]
 where
  infixOperatorFromLexeme :: Lexeme -> (Int -> SExpr -> SExpr -> SExpr) -> Parser (SExpr -> SExpr -> SExpr)
  infixOperatorFromLexeme lexeme operator = do
    _ <- try (lexemeSeparator lexeme)
    pure $ \sexpr1 sexpr2 ->
      operator sexpr2.endLine sexpr1 sexpr2

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
        endLine <- view lineNumberLens <$> getParserState
        pure (mtype, mname, marguments, endLine)
      (\(mtype, mname, marguments, endLine) mcallee -> SEMethodCall{endLine, mcallee, mtype, mname, marguments})

  notParser :: Parser (SExpr -> SExpr)
  notParser =
    recursiveUnaryOperatorParser
      (lexemeSeparator LNot)
      (const $ \sexpr -> SENot sexpr.endLine sexpr)

  tildeParser :: Parser (SExpr -> SExpr)
  tildeParser = do
    recursiveUnaryOperatorParser
      (lexemeSeparator $ LSymbol '~')
      (const $ \sexpr -> SETilde sexpr.endLine sexpr)

  isvoidParser :: Parser (SExpr -> SExpr)
  isvoidParser =
    recursiveUnaryOperatorParser
      (lexemeSeparator LIsVoid)
      (const $ \sexpr -> SEIsVoid sexpr.endLine sexpr)

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
  endLine <- view lineNumberLens <$> getParserState
  pure $ SBinding{endLine, bidentifier, btype, bbody}

assignmentParser :: Parser SExpr
assignmentParser = do
  aid <- objectIdLexemeParser
  _ <- optional whitespace
  _ <- single LAssign
  _ <- optional whitespace
  abody <- expressionParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ SEAssignment{endLine, aid, abody}

selfMethodCallParser :: Parser SExpr
selfMethodCallParser = do
  mname <- objectIdLexemeParser
  startLine <- view lineNumberLens <$> getParserState
  -- TODO: can it be optional?
  _ <- optional whitespace
  marguments <- betweenParenthesis argumentsParser
  endLine <- view lineNumberLens <$> getParserState
  pure $
      SEMethodCall
        { endLine
        , mcallee = SEIdentifier{endLine = startLine, iid = "self"}
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
  endLine <- view lineNumberLens <$> getParserState
  pure $ SEIfThenElse{endLine, iif, ithen, ielse}

whileParser :: Parser SExpr
whileParser = do
  _ <- single LWhile
  wif <- expressionParser
  _ <- single LLoop
  wloop <- expressionParser
  _ <- single LPool
  endLine <- view lineNumberLens <$> getParserState
  pure $ SEWhile{endLine, wif, wloop}

blockParser :: Parser SExpr
blockParser = do
  _ <- single $ LSymbol '{'
  _ <- optional whitespace
  bexpressions <- expressionParser `NE.endBy1` try semicolonSeparator
  _ <- optional whitespace
  _ <- single $ LSymbol '}'
  endLine <- view lineNumberLens <$> getParserState
  pure $ SEBlock{endLine, bexpressions}

letBindingParser :: Parser SExpr
letBindingParser = do
  _ <- single LLet
  _ <- optional whitespace
  lbindings <- bindingParser `NE.sepBy1` try commaSeparator
  _ <- optional whitespace
  _ <- single LIn
  _ <- optional whitespace
  lbody <- expressionParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ SELetIn{endLine, lbindings, lbody}

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
  endLine <- view lineNumberLens <$> getParserState
  pure $ SECase{endLine, cexpr, cprongs}
 where
  caseProngParser :: Parser SCaseProng
  caseProngParser = do
    pidenifier <- objectIdLexemeParser
    _ <- colonSeparator
    ptype <- typeIdLexemeParser
    _ <- separatorParser $ single LDArrow
    pbody <- expressionParser
    endLine <- view lineNumberLens <$> getParserState
    pure $ SCaseProng{endLine, pidenifier, ptype, pbody}

newParser :: Parser SExpr
newParser = do
  _ <- single LNew
  _ <- whitespace
  ntype <- typeIdLexemeParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ SENew{endLine, ntype}

bracketedParser :: Parser SExpr
bracketedParser = do
  bexpr <- betweenParenthesis expressionParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ SEBracketed{endLine, bexpr}

identifierParser :: Parser SExpr
identifierParser = do
  iid <- objectIdLexemeParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ SEIdentifier{endLine, iid}

integerParser :: Parser SExpr
integerParser = do
  iint <- integerLexemeParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ SEInteger{endLine, iint}

stringParser :: Parser SExpr
stringParser = do
  sstring <- stringLexemeParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ SEString{endLine, sstring}

boolParser :: Parser SExpr
boolParser = do
  bbool <- boolLexemeParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ SEBool{endLine, bbool}
