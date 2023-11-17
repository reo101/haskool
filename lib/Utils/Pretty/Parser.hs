{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Redundant <$>" #-}

module Utils.Pretty.Parser (
  lexParseAndPrettyPrint,
) where

import Control.Lens ((^.))
import Data.Either.Extra (fromRight')
import Data.List.NonEmpty qualified as NE
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Lexer (Lexeme (..), lexer)
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

lexParseAndPrettyPrint :: (FilePath, T.Text) -> T.Text
lexParseAndPrettyPrint (sourceFile, sourceCode) =
  T.unlines prettyLines
 where
  lexemes :: [Lexeme]
  lexemes = lexer sourceCode

  -- TODO: after parse
  -- Left err -> pretty print failed lexeme location + info
  -- Right sprogram -> prettyPrintSProgram

  ast :: SProgram
  ast = fromRight' $ parse parser sourceFile lexemes

  prettyLines :: [T.Text]
  prettyLines = prettyPrintSProgram ast

  prettyPrintSProgram :: SProgram -> [T.Text]
  prettyPrintSProgram (SProgram endLine sclasses) =
    mconcat
      [
        [ "#" <> T.pack (show endLine)
        , "_program"
        ]
      , (indent <$>) $
          NE.toList sclasses >>= prettyprintSClass
      ]

  prettyprintSClass :: SClass -> [T.Text]
  prettyprintSClass (SClass endLine name parent features) =
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

  prettyPrintSFeature :: SFeature -> [T.Text]
  prettyPrintSFeature = \case
    SFeatureMember endLine (SBinding endLine' bidentifier btype bbody) ->
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
            prettyPrintMaybeSExpr endLine' bbody
        ]
    SFeatureMethod endLine fidentifier fformals ftype fbody ->
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

  prettyPrintSFormal :: SFormal -> [T.Text]
  prettyPrintSFormal (SFormal endLine fidentifier ftype) =
    [ "#" <> T.pack (show endLine)
    , "_formal"
    , indent $ fidentifier
    , indent $ ftype
    ]

  prettyPrintMaybeSExpr :: Int -> Maybe SExpr -> [T.Text]
  prettyPrintMaybeSExpr endLine = \case
    Just sexpr -> prettyPrintSExpr sexpr
    Nothing -> ["#" <> T.pack (show endLine), "_no_expr", ": _no_type"]

  prettyPrintSExpr :: SExpr -> [T.Text]
  prettyPrintSExpr sexpr = case sexpr of
    SEAssignment endLine aid abody ->
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
          [ ": _no_type"
          ]
        ]
    SEMethodCall endLine mcallee mtype mname marguments ->
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
          [ ": _no_type"
          ]
        ]
    SEIfThenElse endLine iif ithen ielse ->
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
          [ ": _no_type"
          ]
        ]
    SEWhile endLine wif wloop ->
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
          [ ": _no_type"
          ]
        ]
    SEBlock endLine bexpressions ->
      mconcat
        [
          [ "#" <> T.pack (show endLine)
          , "_block"
          ]
        , (indent <$>) $
            NE.toList bexpressions >>= prettyPrintSExpr
        ,
          [ ": _no_type"
          ]
        ]
    SELetIn endLine lbindings lbody ->
      mconcat
        [
          [ "#" <> T.pack (show endLine)
          , "_let"
          ]
        , (indent <$>) $
            let SBinding{endLine = endLine', btype, bidentifier, bbody} NE.:| rest = lbindings
             in mconcat
                  [
                    [ bidentifier
                    , btype
                    ]
                  , prettyPrintMaybeSExpr endLine' bbody
                  , case NE.nonEmpty rest of
                      Just more ->
                        prettyPrintSExpr $ SELetIn endLine more lbody
                      Nothing ->
                        prettyPrintSExpr $ lbody
                  ]
        ,
          [ ": _no_type"
          ]
        ]
    SECase endLine cexpr cprongs ->
      mconcat
        [
          [ "#" <> T.pack (show endLine)
          , "_typcase"
          ]
        , (indent <$>) $
            prettyPrintSExpr cexpr
        , (indent <$>) $
            NE.toList cprongs
              >>= ( \SCaseProng{endLine, ptype, pidenifier, pbody} ->
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
          [ ": _no_type"
          ]
        ]
    SENew endLine ntype ->
      [ "#" <> T.pack (show endLine)
      , "_new"
      , indent $ ntype
      , ": _no_type"
      ]
    SEIsVoid endLine iexpr ->
      mconcat
        [
          [ "#" <> T.pack (show endLine)
          , "_isvoid"
          ]
        , (indent <$>) $
            prettyPrintSExpr iexpr
        ,
          [ ": _no_type"
          ]
        ]
    SEPlus endLine pleft pright ->
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
          [ ": _no_type"
          ]
        ]
    SEMinus endLine mleft mright ->
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
          [ ": _no_type"
          ]
        ]
    SETimes endLine tleft tright ->
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
          [ ": _no_type"
          ]
        ]
    SEDivide endLine dleft dright ->
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
          [ ": _no_type"
          ]
        ]
    SETilde endLine texpr ->
      mconcat
        [
          [ "#" <> T.pack (show endLine)
          , "_neg"
          ]
        , (indent <$>) $
            prettyPrintSExpr texpr
        ,
          [ ": _no_type"
          ]
        ]
    SELt endLine lleft lright ->
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
          [ ": _no_type"
          ]
        ]
    SELte endLine lleft lright ->
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
          [ ": _no_type"
          ]
        ]
    SEEquals endLine eleft eright ->
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
          [ ": _no_type"
          ]
        ]
    SENot endLine nexpr ->
      mconcat
        [
          [ "#" <> T.pack (show endLine)
          , "_comp"
          ]
        , (indent <$>) $
            prettyPrintSExpr nexpr
        ,
          [ ": _no_type"
          ]
        ]
    SEBracketed endLine bexpr ->
      prettyPrintSExpr bexpr
    SEIdentifier endLine iid ->
      [ "#" <> T.pack (show endLine)
      , "_object"
      , indent $ iid
      , ": _no_type"
      ]
    SEInteger endLine iint ->
      [ "#" <> T.pack (show endLine)
      , "_int"
      , indent $ T.pack (show iint)
      , ": _no_type"
      ]
    SEString endLine sstring ->
      [ "#" <> T.pack (show endLine)
      , "_string"
      , indent $ T.pack (show sstring)
      , ": _no_type"
      ]
    SEBool endLine bbool ->
      [ "#" <> T.pack (show endLine)
      , "_bool"
      , indent $ if bbool then "1" else "0"
      , ": _no_type"
      ]

indent :: T.Text -> T.Text
indent = ("  " <>)
