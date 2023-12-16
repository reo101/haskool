{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Redundant <$>" #-}

module Utils.Pretty.Parser (
  lexParseAndPrettyPrint,
  prettyPrintSProgram,
  prettyprintSClass,
  prettyPrintSFeature,
  prettyPrintSFormal,
  prettyPrintMaybeSExpr,
  prettyPrintSExpr,
) where

import Control.Comonad.Cofree (Cofree (..))
import Control.Lens ((^.))
import Data.Either.Extra (fromRight')
import Data.Generics.Labels ()
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Lexer (Lexeme (..), lexer)
import Parser (parser)
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
import System.FilePath.Lens (filename)
import Text.Megaparsec (
  parse,
 )

lexParseAndPrettyPrint :: (FilePath, T.Text) -> T.Text
lexParseAndPrettyPrint (sourceFile, sourceCode) =
  T.unlines prettyLines
 where
  lexemes :: [Lexeme]
  lexemes = lexer sourceCode

  -- TODO: after parse
  -- Left err -> pretty print failed lexeme location + info
  -- Right sprogram -> prettyPrintSProgram

  ast :: SProgram ExtraInfo
  ast = fromRight' $ parse parser sourceFile lexemes

  prettyLines :: [T.Text]
  prettyLines = prettyPrintSProgram sourceFile ast

prettyPrintSProgram :: FilePath -> SProgram ExtraInfo -> [T.Text]
prettyPrintSProgram sourceFile (SProgram ExtraInfo{endLine} sclasses) =
  mconcat
    [
      [ "#" <> T.pack (show endLine)
      , "_program"
      ]
    , (indent <$>) $
        NE.toList sclasses >>= prettyprintSClass sourceFile
    ]

prettyprintSClass :: FilePath -> SClass ExtraInfo -> [T.Text]
prettyprintSClass sourceFile (SClass ExtraInfo{endLine} name parent features) =
  mconcat
    [
      [ "#" <> T.pack (show endLine)
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

prettyPrintSFeature :: SFeature ExtraInfo -> [T.Text]
prettyPrintSFeature = \case
  SFeatureMember ExtraInfo{endLine, typeName} (SBinding extraInfo' bidentifier btype bbody) ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_attr"
        ]
      , (indent <$>) $
          [ bidentifier
          , btype
          ]
      , (indent <$>) $
          prettyPrintMaybeSExpr extraInfo' bbody
      ]
  SFeatureMethod ExtraInfo{endLine, typeName} fidentifier fformals ftype fbody ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
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

prettyPrintSFormal :: SFormal ExtraInfo -> [T.Text]
prettyPrintSFormal (SFormal ExtraInfo{endLine} fidentifier ftype) =
  [ "#" <> T.pack (show endLine)
  , "_formal"
  , indent $ fidentifier
  , indent $ ftype
  ]

prettyPrintMaybeSExpr :: ExtraInfo -> Maybe (SExpr ExtraInfo) -> [T.Text]
prettyPrintMaybeSExpr ExtraInfo{endLine, typeName} = \case
  Just sexpr -> prettyPrintSExpr sexpr
  Nothing -> ["#" <> T.pack (show endLine), "_no_expr", prettyTypeName typeName]

prettyPrintSExpr :: SExpr ExtraInfo -> [T.Text]
prettyPrintSExpr (extraInfo@ExtraInfo{endLine, typeName} :< sexpr) = case sexpr of
  SEAssignment aid abody ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_assign"
        ]
      , (indent <$>) $
          [ aid
          ]
      , (indent <$>) $
          prettyPrintSExpr abody
      ,
        [ prettyTypeName typeName
        ]
      ]
  SEMethodCall mcallee mtype mname marguments ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , maybe "" (const "_static") mtype <> "_dispatch"
        ]
      , (indent <$>) $
          prettyPrintSExpr mcallee
      , (indent <$>) $
          maybe [] pure mtype
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
        [ prettyTypeName typeName
        ]
      ]
  SEIfThenElse iif ithen ielse ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_cond"
        ]
      , (indent <$>) $
          prettyPrintSExpr iif
      , (indent <$>) $
          prettyPrintSExpr ithen
      , (indent <$>) $
          prettyPrintSExpr ielse
      ,
        [ prettyTypeName typeName
        ]
      ]
  SEWhile wif wloop ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_loop"
        ]
      , (indent <$>) $
          prettyPrintSExpr wif
      , (indent <$>) $
          prettyPrintSExpr wloop
      ,
        [ prettyTypeName typeName
        ]
      ]
  SEBlock bexpressions ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_block"
        ]
      , (indent <$>) $
          NE.toList bexpressions >>= prettyPrintSExpr
      ,
        [ prettyTypeName typeName
        ]
      ]
  SELetIn lbinding lbody ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_let"
        ]
      , (indent <$>) $
          let SBinding{extraInfo = extraInfo', btype, bidentifier, bbody} = lbinding
           in mconcat
                [
                  [ bidentifier
                  , btype
                  ]
                , prettyPrintMaybeSExpr extraInfo' bbody
                , prettyPrintSExpr $ lbody
                ]
      ,
        [ prettyTypeName typeName
        ]
      ]
  SECase cexpr cprongs ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_typcase"
        ]
      , (indent <$>) $
          prettyPrintSExpr cexpr
      , (indent <$>) $
          NE.toList cprongs
            >>= ( \SCaseProng{extraInfo = ExtraInfo{endLine, typeName}, ptype, pidenifier, pbody} ->
                    mconcat
                      [
                        [ "#" <> T.pack (show endLine)
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
        [ prettyTypeName typeName
        ]
      ]
  SENew ntype ->
    [ "#" <> T.pack (show endLine)
    , "_new"
    , indent $ ntype
    , prettyTypeName typeName
    ]
  SEIsVoid iexpr ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_isvoid"
        ]
      , (indent <$>) $
          prettyPrintSExpr iexpr
      ,
        [ prettyTypeName typeName
        ]
      ]
  SEPlus pleft pright ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_plus"
        ]
      , (indent <$>) $
          prettyPrintSExpr pleft
      , (indent <$>) $
          prettyPrintSExpr pright
      ,
        [ prettyTypeName typeName
        ]
      ]
  SEMinus mleft mright ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_sub"
        ]
      , (indent <$>) $
          prettyPrintSExpr mleft
      , (indent <$>) $
          prettyPrintSExpr mright
      ,
        [ prettyTypeName typeName
        ]
      ]
  SETimes tleft tright ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_mul"
        ]
      , (indent <$>) $
          prettyPrintSExpr tleft
      , (indent <$>) $
          prettyPrintSExpr tright
      ,
        [ prettyTypeName typeName
        ]
      ]
  SEDivide dleft dright ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_divide"
        ]
      , (indent <$>) $
          prettyPrintSExpr dleft
      , (indent <$>) $
          prettyPrintSExpr dright
      ,
        [ prettyTypeName typeName
        ]
      ]
  SETilde texpr ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_neg"
        ]
      , (indent <$>) $
          prettyPrintSExpr texpr
      ,
        [ prettyTypeName typeName
        ]
      ]
  SELt lleft lright ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_lt"
        ]
      , (indent <$>) $
          prettyPrintSExpr lleft
      , (indent <$>) $
          prettyPrintSExpr lright
      ,
        [ prettyTypeName typeName
        ]
      ]
  SELte lleft lright ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_lte"
        ]
      , (indent <$>) $
          prettyPrintSExpr lleft
      , (indent <$>) $
          prettyPrintSExpr lright
      ,
        [ prettyTypeName typeName
        ]
      ]
  SEEquals eleft eright ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_eq"
        ]
      , (indent <$>) $
          prettyPrintSExpr eleft
      , (indent <$>) $
          prettyPrintSExpr eright
      ,
        [ prettyTypeName typeName
        ]
      ]
  SENot nexpr ->
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_comp"
        ]
      , (indent <$>) $
          prettyPrintSExpr nexpr
      ,
        [ prettyTypeName typeName
        ]
      ]
  SEBracketed bexpr ->
    prettyPrintSExpr bexpr
  SEIdentifier iid ->
    [ "#" <> T.pack (show endLine)
    , "_object"
    , indent $ iid
    , prettyTypeName typeName
    ]
  SEInteger iint ->
    [ "#" <> T.pack (show endLine)
    , "_int"
    , indent $ T.pack (show iint)
    , prettyTypeName typeName
    ]
  SEString sstring ->
    [ "#" <> T.pack (show endLine)
    , "_string"
    , indent $ T.pack (show sstring)
    , prettyTypeName typeName
    ]
  SEBool bbool ->
    [ "#" <> T.pack (show endLine)
    , "_bool"
    , indent $ if bbool then "1" else "0"
    , prettyTypeName typeName
    ]

indent :: T.Text -> T.Text
indent = ("  " <>)

prettyTypeName :: Maybe T.Text -> T.Text
prettyTypeName Nothing = ": _no_type"
prettyTypeName (Just typeName) = ": " <> typeName
