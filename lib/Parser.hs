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
import Control.Comonad.Cofree (Cofree (..))
import Control.Lens (Iso', iso, lens, view, (+~))
import Control.Lens.Combinators (Lens')
import Control.Monad (join, void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Control.Monad.Combinators.NonEmpty qualified as NE (endBy1, sepBy1)
import Data.Generics.Labels ()
import Data.List.NonEmpty as NE (last)
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Void (Void)
import Lexer (Lexeme (..), lexer)
import Parser.Types (
  ExtraInfo (..),
  SBinding (..),
  SCaseProng (..),
  SClass (..),
  SExpr,
  SExprF (..),
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
import Text.Megaparsec.Error qualified

type Parser = Parsec Void [Lexeme]

choice :: [Parser a] -> Parser a
choice = Control.Applicative.Combinators.choice . (try <$>)

---

parser :: Parser (SProgram ExtraInfo)
parser = programParser

programParser :: Parser (SProgram ExtraInfo)
programParser = do
  pclasses <- classParser `NE.endBy1` try semicolonSeparator
  let endLine = (NE.last pclasses).extraInfo.endLine
  pure $ SProgram{extraInfo = ExtraInfo{endLine, typeName = Nothing}, pclasses}

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

classParser :: Parser (SClass ExtraInfo)
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
  pure $ SClass{extraInfo = ExtraInfo{endLine, typeName = Nothing}, name, parent, features}

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

lineNumberLens :: Lens' (State s e) Int
lineNumberLens = #statePosState . #pstateSourcePos . #sourceLine . iso unPos mkPos

argumentsParser :: Parser [SExpr ExtraInfo]
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

formalParser :: Parser (SFormal ExtraInfo)
formalParser = do
  fidentifier <- objectIdLexemeParser
  ftype <- typeIdLexemeParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ SFormal{extraInfo = ExtraInfo{endLine, typeName = Nothing}, fidentifier, ftype}

featureParser :: Parser (SFeature ExtraInfo)
featureParser = choice [featureMethodParser, featureMemberParser]
 where
  featureMethodParser :: Parser (SFeature ExtraInfo)
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
    pure $ SFeatureMethod{extraInfo = ExtraInfo{endLine, typeName = Nothing}, fidentifier, fformals, ftype, fbody}
  featureMemberParser :: Parser (SFeature ExtraInfo)
  featureMemberParser = do
    fbinding <- bindingParser
    endLine <- view lineNumberLens <$> getParserState
    pure $ SFeatureMember{extraInfo = ExtraInfo{endLine, typeName = Nothing}, fbinding}

operators :: [[Operator Parser (SExpr ExtraInfo)]]
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
  infixOperatorFromLexeme ::
    Lexeme ->
    ( SExprF ExtraInfo (SExpr ExtraInfo) ->
      SExprF ExtraInfo (SExpr ExtraInfo) ->
      SExprF ExtraInfo (SExprF ExtraInfo (SExpr ExtraInfo))
    ) ->
    Parser
      ( SExpr ExtraInfo ->
        SExpr ExtraInfo ->
        SExpr ExtraInfo
      )
  infixOperatorFromLexeme lexeme operator = do
    _ <- try (lexemeSeparator lexeme)
    pure $ \(extraInfo :< sexpr1') (_ :< sexpr2') ->
      (extraInfo :<) $ ((extraInfo :<) <$>) $ operator sexpr1' sexpr2'

  recursiveUnaryOperatorParser ::
    Parser a ->
    ( a ->
      SExpr ExtraInfo ->
      SExpr ExtraInfo
    ) ->
    Parser
      ( SExpr ExtraInfo ->
        SExpr ExtraInfo
      )
  recursiveUnaryOperatorParser p c = do
    a <- p
    maybeRec <- optional $ recursiveUnaryOperatorParser p c
    pure $
      fromMaybe id maybeRec . c a

  methodCallSuffixParser :: Parser (SExpr ExtraInfo -> SExpr ExtraInfo)
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
      (\(mtype, mname, marguments, endLine) mcallee -> ExtraInfo{endLine, typeName = Nothing} :< SEMethodCall{mcallee, mtype, mname, marguments})

  notParser :: Parser (SExpr ExtraInfo -> SExpr ExtraInfo)
  notParser =
    recursiveUnaryOperatorParser
      (lexemeSeparator LNot)
      (const $ \sexpr@(extraInfo :< _) -> extraInfo :< SENot sexpr)

  tildeParser :: Parser (SExpr ExtraInfo -> SExpr ExtraInfo)
  tildeParser = do
    recursiveUnaryOperatorParser
      (lexemeSeparator $ LSymbol '~')
      (const $ \sexpr@(extraInfo :< _) -> extraInfo :< SETilde sexpr)

  isvoidParser :: Parser (SExpr ExtraInfo -> SExpr ExtraInfo)
  isvoidParser =
    recursiveUnaryOperatorParser
      (lexemeSeparator LIsVoid)
      (const $ \sexpr@(extraInfo :< _) -> extraInfo :< SEIsVoid sexpr)

expressionParser :: Parser (SExpr ExtraInfo)
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

bindingParser :: Parser (SBinding ExtraInfo)
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
  pure $ SBinding{extraInfo = ExtraInfo{endLine, typeName = Nothing}, bidentifier, btype, bbody}

assignmentParser :: Parser (SExpr ExtraInfo)
assignmentParser = do
  aid <- objectIdLexemeParser
  _ <- optional whitespace
  _ <- single LAssign
  _ <- optional whitespace
  abody <- expressionParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ ExtraInfo{endLine, typeName = Nothing} :< SEAssignment{aid, abody}

selfMethodCallParser :: Parser (SExpr ExtraInfo)
selfMethodCallParser = do
  mname <- objectIdLexemeParser
  startLine <- view lineNumberLens <$> getParserState
  -- TODO: can it be optional?
  _ <- optional whitespace
  marguments <- betweenParenthesis argumentsParser
  endLine <- view lineNumberLens <$> getParserState
  pure $
    ExtraInfo{endLine, typeName = Nothing}
      :< SEMethodCall
        { mcallee = ExtraInfo{endLine = startLine, typeName = Nothing} :< SEIdentifier{iid = "self"}
        , mtype = Nothing
        , mname
        , marguments
        }

ifThenElseParser :: Parser (SExpr ExtraInfo)
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
  pure $ ExtraInfo{endLine, typeName = Nothing} :< SEIfThenElse{iif, ithen, ielse}

whileParser :: Parser (SExpr ExtraInfo)
whileParser = do
  _ <- single LWhile
  wif <- expressionParser
  _ <- single LLoop
  wloop <- expressionParser
  _ <- single LPool
  endLine <- view lineNumberLens <$> getParserState
  pure $ ExtraInfo{endLine, typeName = Nothing} :< SEWhile{wif, wloop}

blockParser :: Parser (SExpr ExtraInfo)
blockParser = do
  _ <- single $ LSymbol '{'
  _ <- optional whitespace
  bexpressions <- expressionParser `NE.endBy1` try semicolonSeparator
  _ <- optional whitespace
  _ <- single $ LSymbol '}'
  endLine <- view lineNumberLens <$> getParserState
  pure $ ExtraInfo{endLine, typeName = Nothing} :< SEBlock{bexpressions}

letBindingParser :: Parser (SExpr ExtraInfo)
letBindingParser = do
  _ <- single LLet
  _ <- optional whitespace
  lbindings <- bindingParser `NE.sepBy1` try commaSeparator
  _ <- optional whitespace
  _ <- single LIn
  _ <- optional whitespace
  lbody <- expressionParser
  endLine <- view lineNumberLens <$> getParserState
  pure $
    foldr
      ( \lbinding acc ->
          ExtraInfo{endLine, typeName = Nothing}
            :< SELetIn{lbinding, lbody = acc}
      )
      lbody
      lbindings

caseParser :: Parser (SExpr ExtraInfo)
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
  pure $ ExtraInfo{endLine, typeName = Nothing} :< SECase{cexpr, cprongs}
 where
  caseProngParser :: Parser (SCaseProng ExtraInfo)
  caseProngParser = do
    pidenifier <- objectIdLexemeParser
    _ <- colonSeparator
    ptype <- typeIdLexemeParser
    _ <- separatorParser $ single LDArrow
    pbody <- expressionParser
    endLine <- view lineNumberLens <$> getParserState
    pure $ SCaseProng{extraInfo = ExtraInfo{endLine, typeName = Nothing}, pidenifier, ptype, pbody}

newParser :: Parser (SExpr ExtraInfo)
newParser = do
  _ <- single LNew
  _ <- whitespace
  ntype <- typeIdLexemeParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ ExtraInfo{endLine, typeName = Nothing} :< SENew{ntype}

bracketedParser :: Parser (SExpr ExtraInfo)
bracketedParser = do
  bexpr <- betweenParenthesis expressionParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ ExtraInfo{endLine, typeName = Nothing} :< SEBracketed{bexpr}

identifierParser :: Parser (SExpr ExtraInfo)
identifierParser = do
  iid <- objectIdLexemeParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ ExtraInfo{endLine, typeName = Nothing} :< SEIdentifier{iid}

integerParser :: Parser (SExpr ExtraInfo)
integerParser = do
  iint <- integerLexemeParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ ExtraInfo{endLine, typeName = Nothing} :< SEInteger{iint}

stringParser :: Parser (SExpr ExtraInfo)
stringParser = do
  sstring <- stringLexemeParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ ExtraInfo{endLine, typeName = Nothing} :< SEString{sstring}

boolParser :: Parser (SExpr ExtraInfo)
boolParser = do
  bbool <- boolLexemeParser
  endLine <- view lineNumberLens <$> getParserState
  pure $ ExtraInfo{endLine, typeName = Nothing} :< SEBool{bbool}
