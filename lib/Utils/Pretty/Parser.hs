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
  ExtraInfo (..),
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

  prettyPrintSFormal :: SFormal -> [T.Text]
  prettyPrintSFormal (SFormal endLine fidentifier ftype) =
    [ "#" <> T.pack (show endLine)
    , "_formal"
    , indent $ fidentifier
    , indent $ ftype
    ]

  prettyPrintMaybeSExpr :: ExtraInfo -> Maybe SExpr -> [T.Text]
  prettyPrintMaybeSExpr ExtraInfo{endLine, typeName} = \case
    Just sexpr -> prettyPrintSExpr sexpr
    Nothing -> ["#" <> T.pack (show endLine), "_no_expr", prettyTypeName typeName]

  prettyPrintSExpr :: SExpr -> [T.Text]
  prettyPrintSExpr sexpr = case sexpr of
    SEAssignment ExtraInfo{endLine, typeName} aid abody ->
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
    SEMethodCall ExtraInfo{endLine, typeName} mcallee mtype mname marguments ->
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
    SEIfThenElse ExtraInfo{endLine, typeName} iif ithen ielse ->
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
    SEWhile ExtraInfo{endLine, typeName} wif wloop ->
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
    SEBlock ExtraInfo{endLine, typeName} bexpressions ->
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
    SELetIn ExtraInfo{endLine, typeName} lbindings lbody ->
      mconcat
        [
          [ "#" <> T.pack (show endLine)
          , "_let"
          ]
        , (indent <$>) $
            let SBinding{extraInfo = extraInfo', btype, bidentifier, bbody} NE.:| rest = lbindings
             in mconcat
                  [
                    [ bidentifier
                    , btype
                    ]
                  , prettyPrintMaybeSExpr extraInfo' bbody
                  , case NE.nonEmpty rest of
                      Just more ->
                        prettyPrintSExpr $ SELetIn ExtraInfo{endLine, typeName} more lbody
                      Nothing ->
                        prettyPrintSExpr $ lbody
                  ]
        ,
          [ prettyTypeName typeName
          ]
        ]
    SECase ExtraInfo{endLine, typeName} cexpr cprongs ->
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
    SENew ExtraInfo{endLine, typeName} ntype ->
      [ "#" <> T.pack (show endLine)
      , "_new"
      , indent $ ntype
      , prettyTypeName typeName
      ]
    SEIsVoid ExtraInfo{endLine, typeName} iexpr ->
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
    SEPlus ExtraInfo{endLine, typeName} pleft pright ->
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
    SEMinus ExtraInfo{endLine, typeName} mleft mright ->
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
    SETimes ExtraInfo{endLine, typeName} tleft tright ->
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
    SEDivide ExtraInfo{endLine, typeName} dleft dright ->
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
    SETilde ExtraInfo{endLine, typeName} texpr ->
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
    SELt ExtraInfo{endLine, typeName} lleft lright ->
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
    SELte ExtraInfo{endLine, typeName} lleft lright ->
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
    SEEquals ExtraInfo{endLine, typeName} eleft eright ->
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
    SENot ExtraInfo{endLine, typeName} nexpr ->
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
    SEBracketed ExtraInfo{endLine, typeName} bexpr ->
      prettyPrintSExpr bexpr
    SEIdentifier ExtraInfo{endLine, typeName} iid ->
      [ "#" <> T.pack (show endLine)
      , "_object"
      , indent $ iid
      , prettyTypeName typeName
      ]
    SEInteger ExtraInfo{endLine, typeName} iint ->
      [ "#" <> T.pack (show endLine)
      , "_int"
      , indent $ T.pack (show iint)
      , prettyTypeName typeName
      ]
    SEString ExtraInfo{endLine, typeName} sstring ->
      [ "#" <> T.pack (show endLine)
      , "_string"
      , indent $ T.pack (show sstring)
      , prettyTypeName typeName
      ]
    SEBool ExtraInfo{endLine, typeName} bbool ->
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
