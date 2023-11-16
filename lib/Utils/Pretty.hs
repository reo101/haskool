{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Redundant <$>" #-}
module Utils.Pretty (
  wrapAndIntercalate,
  lexAndPrettyPrint,
  lexParseAndPrettyPrint,
  prettyPrintLexeme,
) where

import Control.Lens (Field1 (_1), Field2 (_2), use, (%=), (+=), (^.))
import Control.Monad.State (State, evalState)
import Data.Char (toLower)
import Data.Either.Extra (fromRight')
import Data.List (intercalate)
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Text qualified as T
import Lexer (Lexeme (..), lexer)
import Numeric (showOct)
import Parser (parser)
import Parser.Types (
  SBinding (..),
  SCaseProng (..),
  SClass (..),
  SExpr (..),
  SFeature (..),
  SFormal (..),
  SProgram (..),
 )
import System.FilePath.Lens (filename)
import Text.Megaparsec (
  parse,
 )
import Text.Printf (printf)

wrapAndIntercalate :: String -> String -> String -> [String] -> String
wrapAndIntercalate left middle right xs = left ++ intercalate middle xs ++ right

lexParseAndPrettyPrint :: (FilePath, T.Text) -> T.Text
lexParseAndPrettyPrint (sourceFile, sourceCode) =
  T.unlines prettyLines
 where
  lexemes :: [Lexeme]
  lexemes = lexer sourceCode

  ast :: SProgram
  ast = fromRight' $ parse parser sourceFile lexemes

  prettyLines :: [T.Text]
  prettyLines = prettyPrintSProgram ast

  prettyPrintSProgram :: SProgram -> [T.Text]
  prettyPrintSProgram (SProgram sclasses) =
    mconcat
      [
        [ "#1"
        , "_program"
        ]
      , (indent <$>) $
          NE.toList sclasses >>= prettyprintSClass
      ]

  prettyprintSClass :: SClass -> [T.Text]
  prettyprintSClass (SClass name parent features) =
    mconcat
      [
        [ "#1"
        , "_class"
        ]
      , indent
          <$> [ name
              ]
      , indent
          <$> pure (fromMaybe "Object" parent)
      , indent
          <$> [ T.pack $ show $ sourceFile ^. filename
              , "("
              ]
      , (indent <$>) $
          features >>= prettyPrintSFeature
      ,
        [ indent $ ")"
        ]
      ]

  prettyPrintSFeature :: SFeature -> [T.Text]
  prettyPrintSFeature = \case
    SFeatureMember (SBinding bidentifier btype bbody) ->
      mconcat
        [
          [ "#1"
          , "_attr"
          ]
        , (indent <$>) $
            [ bidentifier
            , btype
            ]
        , (indent <$>) $
            maybe ["#1", "_no_expr", ": _no_type"] prettyPrintSExpr bbody
        ]
    SFeatureMethod fidentifier fformals ftype fbody ->
      mconcat
        [
          [ "#1"
          , "_method"
          ]
        , (indent <$>) $
            [ fidentifier
            , ftype
            ]
        , (indent <$>) $
            fformals >>= prettyPrintSFormal
        , (indent <$>) $
            prettyPrintSExpr fbody
        ]

  prettyPrintSFormal :: SFormal -> [T.Text]
  prettyPrintSFormal (SFormal fidentifier ftype) =
    [ "#1" -- TODO: Handle line indexing
    , "_formal"
    , indent $ fidentifier
    , indent $ ftype
    ]

  prettyPrintSExpr :: SExpr -> [T.Text]
  prettyPrintSExpr sexpr = case sexpr of
    SEAssignment aid abody ->
      mconcat
        [
          [ "#1"
          , "_assign"
          ]
        , (indent <$>) $
            [ aid
            ]
        , (indent <$>) $
            prettyPrintSExpr abody
        ,
          [ ": _no_type"
          ]
        ]
    SEMethodCall mcallee mtype mname marguments ->
      mconcat
        [
          [ "#1"
          , "_dispatch"
          ]
        , (indent <$>) $
            prettyPrintSExpr mcallee
        , indent
            <$> [ mname
                , "("
                ]
        , (indent <$>) $
            marguments >>= prettyPrintSExpr
        , indent
            <$> [ ")"
                ]
        ,
          [ ": _no_type"
          ]
        ]
    SEIfThenElse iif ithen ielse ->
      mconcat
        [
          [ "#1"
          , "_cond"
          ]
        , (indent <$>) $
            prettyPrintSExpr iif
        , (indent <$>) $
            prettyPrintSExpr ithen
        , (indent <$>) $
            prettyPrintSExpr ielse
        ,
          [ ": _no_type"
          ]
        ]
    SEWhile wif wloop ->
      mconcat
        [
          [ "#1"
          , "_loop"
          ]
        , (indent <$>) $
            prettyPrintSExpr wif
        , (indent <$>) $
            prettyPrintSExpr wloop
        ,
          [ ": _no_type"
          ]
        ]
    SEBlock bexpressions ->
      mconcat
        [
          [ "#1"
          , "_block"
          ]
        , (indent <$>) $
            NE.toList bexpressions >>= prettyPrintSExpr
        ,
          [ ": _no_type"
          ]
        ]
    SELetIn lbindings lbody ->
      mconcat
        [
          [ "#1"
          , "_let"
          ]
        , (indent <$>) $
            NE.toList lbindings
              >>= ( \SBinding{btype, bidentifier, bbody} ->
                      mconcat
                        [
                          [ bidentifier
                          , btype
                          ]
                        , maybe [] prettyPrintSExpr bbody
                        ]
                  )
        , (indent <$>) $
            prettyPrintSExpr lbody
        ,
          [ ": _no_type"
          ]
        ]
    SECase cexpr cprongs ->
      mconcat
        [
          [ "#1"
          , "_typcase"
          ]
        , (indent <$>) $
            prettyPrintSExpr cexpr
        , (indent <$>) $
            NE.toList cprongs
              >>= ( \SCaseProng{ptype, pidenifier, pbody} ->
                      mconcat
                        [
                          [ "#1"
                          , "_branch"
                          ]
                        , (indent <$>) $
                            [ pidenifier
                            , ptype
                            ]
                        , (indent <$>) $
                            prettyPrintSExpr pbody
                        ]
                  )
        ,
          [ ": _no_type"
          ]
        ]
    SENew ntype ->
      [ "#1"
      , "_new"
      , indent $ ntype
      , ": _no_type"
      ]
    SEIsVoid iexpr ->
      mconcat
        [
          [ "#1"
          , "_isvoid"
          ]
        , (indent <$>) $
            prettyPrintSExpr iexpr
        ,
          [ ": _no_type"
          ]
        ]
    SEPlus pleft pright ->
      mconcat
        [
          [ "#1"
          , "_plus"
          ]
        , (indent <$>) $
            prettyPrintSExpr pleft
        , (indent <$>) $
            prettyPrintSExpr pright
        ,
          [ ": _no_type"
          ]
        ]
    SEMinus mleft mright ->
      mconcat
        [
          [ "#1"
          , "_minus"
          ]
        , (indent <$>) $
            prettyPrintSExpr mleft
        , (indent <$>) $
            prettyPrintSExpr mright
        ,
          [ ": _no_type"
          ]
        ]
    SETimes tleft tright ->
      mconcat
        [
          [ "#1"
          , "_mul"
          ]
        , (indent <$>) $
            prettyPrintSExpr tleft
        , (indent <$>) $
            prettyPrintSExpr tright
        ,
          [ ": _no_type"
          ]
        ]
    SEDivide dleft dright ->
      mconcat
        [
          [ "#1"
          , "_divide"
          ]
        , (indent <$>) $
            prettyPrintSExpr dleft
        , (indent <$>) $
            prettyPrintSExpr dright
        ,
          [ ": _no_type"
          ]
        ]
    SETilde texpr ->
      mconcat
        [
          [ "#1"
          , "_neg"
          ]
        , (indent <$>) $
            prettyPrintSExpr texpr
        ,
          [ ": _no_type"
          ]
        ]
    SELt lleft lright ->
      mconcat
        [
          [ "#1"
          , "_lt"
          ]
        , (indent <$>) $
            prettyPrintSExpr lleft
        , (indent <$>) $
            prettyPrintSExpr lright
        ,
          [ ": _no_type"
          ]
        ]
    SELte lleft lright ->
      mconcat
        [
          [ "#1"
          , "_lte"
          ]
        , (indent <$>) $
            prettyPrintSExpr lleft
        , (indent <$>) $
            prettyPrintSExpr lright
        ,
          [ ": _no_type"
          ]
        ]
    SEEquals eleft eright ->
      mconcat
        [
          [ "#1"
          , "_eq"
          ]
        , (indent <$>) $
            prettyPrintSExpr eleft
        , (indent <$>) $
            prettyPrintSExpr eright
        ,
          [ ": _no_type"
          ]
        ]
    SENot nexpr ->
      mconcat
        [
          [ "#1"
          , "_not"
          ]
        , (indent <$>) $
            prettyPrintSExpr nexpr
        ,
          [ ": _no_type"
          ]
        ]
    SEBracketed bexpr ->
      prettyPrintSExpr bexpr
    SEIdentifier iid ->
      [ "#1"
      , "_object"
      , indent $ iid
      , ": _no_type"
      ]
    SEInteger iint ->
      [ "#1"
      , "_int"
      , indent $ T.pack (show iint)
      , ": _no_type"
      ]
    SEString sstring ->
      [ "#1"
      , "_string"
      , indent $ T.pack (show sstring)
      , ": _no_type"
      ]
    SEBool bbool ->
      [ "#1"
      , "_bool"
      , indent $ if bbool then "1" else "0"
      , ": _no_type"
      ]

indent :: T.Text -> T.Text
indent = ("  " <>)

lexAndPrettyPrint :: (FilePath, T.Text) -> T.Text
lexAndPrettyPrint (sourceFile, sourceCode) =
  T.unlines $ header : prettyLines
 where
  lexemes :: [Lexeme]
  lexemes = lexer sourceCode

  prettyLines :: [T.Text]
  prettyLines = evalState linenize (1, lexemes)

  header :: T.Text
  header =
    T.pack $
      printf
        "#name \"%s\""
        (sourceFile ^. filename)

linenize :: State (Int, [Lexeme]) [T.Text]
linenize = do
  maybeLexeme <- takeLexeme
  case maybeLexeme of
    Just token -> do
      line <- use _1
      process <- processLexeme line token
      maybe id (:) process <$> linenize
    Nothing -> do
      pure []
 where
  takeLexeme :: State (Int, [Lexeme]) (Maybe Lexeme)
  takeLexeme = do
    lexemes <- use _2
    case listToMaybe lexemes of
      Nothing -> do
        pure Nothing
      Just token -> do
        _2 %= tail
        pure $ Just token

numberedLexemeToPretty :: (Int, Lexeme) -> T.Text
numberedLexemeToPretty (line, token) =
  T.pack $
    printf
      "#%s %s"
      (show line)
      (prettyPrintLexeme token)

processLexeme :: Int -> Lexeme -> State (Int, [Lexeme]) (Maybe T.Text)
processLexeme line token = do
  _1 += skipLines
  pure maybePrintedLexeme
 where
  (skipLines, maybePrintedLexeme) = case token of
    LNewLine ->
      (1, Nothing)
    LComment n ->
      (n, Nothing)
    LWhitespace ->
      (0, Nothing)
    LString n _ ->
      (n, Just $ numberedLexemeToPretty (line + n, token))
    LError showLine skipLines _ ->
      (skipLines, Just $ numberedLexemeToPretty (line + showLine, token))
    _ ->
      (0, Just $ numberedLexemeToPretty (line, token))

charToMaybeOctal :: Char -> String
charToMaybeOctal ch =
  if
      | fromEnum ch `elem` escapedCharacters ->
          printf "\\%03s" (showOct (fromEnum ch) "")
      | ch `elem` ['\\', '"'] ->
          printf "\\%s" [ch]
      | otherwise -> case ch of
          '\n' -> printf "\\n"
          '\t' -> printf "\\t"
          '\b' -> printf "\\b"
          '\f' -> printf "\\f"
          _ -> printf "%s" [ch]
 where
  escapedCharacters =
    [ 0o00
    , 0o01
    , 0o02
    , 0o03
    , 0o04
    , 0o13
    , 0o15
    , 0o22
    , 0o33
    ]

prettyPrintLexeme :: Lexeme -> String
prettyPrintLexeme = \case
  LInteger i -> printf "INT_CONST %s" (show i)
  LString _ s -> printf "STR_CONST \"%s\"" (T.unpack s >>= charToMaybeOctal)
  -- LComment n -> undefined
  -- LEOF -> undefined
  LClass -> printf "CLASS"
  LIf -> printf "IF"
  LThen -> printf "THEN"
  LElse -> printf "ELSE"
  LFi -> printf "FI"
  LInherits -> printf "INHERITS"
  LIsVoid -> printf "ISVOID"
  LLet -> printf "LET"
  LIn -> printf "IN"
  LWhile -> printf "WHILE"
  LLoop -> printf "LOOP"
  LPool -> printf "POOL"
  LCase -> printf "CASE"
  LEsac -> printf "ESAC"
  LNew -> printf "NEW"
  LOf -> printf "OF"
  LNot -> printf "NOT"
  LBool p -> printf "BOOL_CONST %s" (T.pack $ toLower <$> show p)
  LTypeId i -> printf "TYPEID %s" i
  LObjectId i -> printf "OBJECTID %s" i
  LDArrow -> printf "DARROW"
  LLessEqual -> printf "LE"
  LAssign -> printf "ASSIGN"
  -- LWhitespace -> undefined
  -- LNewLine -> undefined
  LSymbol ch -> printf "'%s'" [ch]
  LError _ _ e -> printf "ERROR \"%s\"" (T.unpack e >>= charToMaybeOctal)
  _ -> "### SHOULD NOT BE PRETTY PRINTED ###"
