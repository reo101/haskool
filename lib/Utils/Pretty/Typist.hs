{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# HLINT ignore "Redundant pure" #-}

module Utils.Pretty.Typist (
  lexParseTypeAndPrettyPrint,
) where

import Control.Comonad.Cofree (Cofree (..))
import Control.Lens ((^.))
import Control.Lens.Internal.Deque qualified as M
import Control.Monad.State (StateT (runStateT), evalStateT)
import Data.Bifunctor (first)
import Data.Either.Extra (fromRight')
import Data.Generics.Labels ()
import Data.IntMap qualified as M
import Data.List.NonEmpty qualified as NE
import Data.Map qualified as Map
import Data.Map.NonEmpty qualified as NEM
import Data.Maybe (fromMaybe)
import Data.Text qualified as T
import Debug.Trace (trace, traceShow, traceShowM)
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
import Text.Printf (printf)
import Typist (typecheckSProgram)
import Typist.Types (
  Class,
  Context (..),
  Identifier,
  O,
  Tree,
  Type,
 )
import Utils.Algorithms (
  allClassesWithParents,
  classHierarchyGraph,
  classHierarchyTree,
  dagToTree,
  extendO,
  extractInfo,
  findFirstDuplicate,
  lca,
  subtype,
 )
import Utils.Pretty.Parser (
  prettyPrintMaybeSExpr,
  prettyPrintSExpr,
  prettyPrintSFeature,
  prettyPrintSFormal,
  prettyPrintSProgram,
  prettyprintSClass,
 )

-- >>> lexParseTypeAndPrettyPrint $ (,) "" prg
-- "\n"

-- >>> traceShow 5 6
-- 6

defaultProgram :: SProgram ExtraInfo
defaultProgram =
  SProgram
    { pclasses
    , extraInfo =
        ExtraInfo
          { typeName = Nothing
          , endLine = 1
          }
    }
 where
  pclasses =
    NE.fromList
      [ SClass
          { -- TODO: Maybe fix later
            parent = Nothing
          , name = "Object"
          , features =
              [ SFeatureMethod
                  { extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}
                  , fidentifier = "abort"
                  , fformals =
                      []
                  , ftype = "Object"
                  , fbody = undefined
                  }
              , SFeatureMethod
                  { extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}
                  , fidentifier = "type_name"
                  , fformals =
                      []
                  , ftype = "String"
                  , fbody = undefined
                  }
              , SFeatureMethod
                  { extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}
                  , fidentifier = "copy"
                  , fformals =
                      []
                  , ftype = "SELF_TYPE"
                  , fbody = undefined
                  }
              ]
          , extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}
          }
      , SClass
          { parent = Just "Object"
          , name = "Int"
          , features = []
          , extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}
          }
      , SClass
          { parent = Just "Object"
          , name = "Bool"
          , features = []
          , extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}
          }
      , SClass
          { parent = Just "Object"
          , name = "String"
          , features =
              [ SFeatureMethod
                  { extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}
                  , fidentifier = "length"
                  , fformals =
                      []
                  , ftype = "Int"
                  , fbody = undefined
                  }
              , SFeatureMethod
                  { extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}
                  , fidentifier = "concat"
                  , fformals =
                      [SFormal{extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}, fidentifier = "arg", ftype = "String"}]
                  , ftype = "Int"
                  , fbody = undefined
                  }
              , SFeatureMethod
                  { extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}
                  , fidentifier = "substr"
                  , fformals =
                      [ SFormal{extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}, fidentifier = "arg1", ftype = "Int"}
                      , SFormal{extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}, fidentifier = "arg2", ftype = "Int"}
                      ]
                  , ftype = "Int"
                  , fbody = undefined
                  }
              ]
          , extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}
          }
      , SClass
          { parent = Just "Object"
          , name = "IO"
          , features =
              [ SFeatureMethod
                  { extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}
                  , fidentifier = "out_string"
                  , fformals =
                      [ SFormal
                          { extraInfo =
                              ExtraInfo
                                { typeName = Just "Object"
                                , endLine = 1
                                }
                          , fidentifier = "arg"
                          , ftype = "String"
                          }
                      ]
                  , ftype = "SELF_TYPE"
                  , fbody = undefined
                  }
              , SFeatureMethod
                  { extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}
                  , fidentifier = "out_int"
                  , fformals =
                      [ SFormal
                          { extraInfo =
                              ExtraInfo
                                { typeName = Just "Object"
                                , endLine = 1
                                }
                          , fidentifier = "arg"
                          , ftype = "Int"
                          }
                      ]
                  , ftype = "SELF_TYPE"
                  , fbody = undefined
                  }
              , SFeatureMethod
                  { extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}
                  , fidentifier = "in_string"
                  , fformals =
                      []
                  , ftype = "String"
                  , fbody = undefined
                  }
              , SFeatureMethod
                  { extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}
                  , fidentifier = "in_int"
                  , fformals =
                      []
                  , ftype = "Int"
                  , fbody = undefined
                  }
              ]
          , extraInfo = ExtraInfo{typeName = Nothing, endLine = 1}
          }
      ]

lexParseTypeAndPrettyPrint :: (FilePath, T.Text) -> T.Text
lexParseTypeAndPrettyPrint (sourceFile, sourceCode) =
  -- traceShow lexemes $
  -- trace (T.unpack . T.unlines $ prettyPrintSProgram sourceFile ast) $
  -- trace (T.unpack . T.unlines $ prettyPrintSProgram sourceFile $ fromRight' typedAst) $
  T.unlines prettyLines
 where
  lexemes :: [Lexeme]
  lexemes = lexer sourceCode

  ast :: SProgram ExtraInfo
  ast = fromRight' $ parse parser sourceFile lexemes

  initialContext :: Either String Context
  initialContext = do
    let programs = NE.fromList [defaultProgram, ast]
    classParentHirearchy <- allClassesWithParents programs
    traceShowM "ALL CLASSES WITH PARENTS"
    traceShowM classParentHirearchy

    let identifierTypes = Map.empty
    let methodTypes = Map.fromList $ extractInfo $ programs
    let currentClass = error "Not inside a class yet"
    let classHierarchyG = classHierarchyGraph $ classParentHirearchy
    -- traceShowM classHierarchyG

    classHierarchy <-
      case classHierarchyTree classHierarchyG of
        Left classesCycle -> Left $ printf "Class cycle found: %s" $ show classesCycle
        Right tree -> Right tree

    let errors = []

    pure $
      Context
        { identifierTypes
        , methodTypes
        , currentClass
        , classHierarchy
        , programs
        , classParentHirearchy
        , errors
        }

  typedAst :: Either [T.Text] (SProgram ExtraInfo)
  typedAst = do
    (res, finalContext) <- first (pure . T.pack) $ (runStateT (typecheckSProgram ast) =<< initialContext)
    case finalContext ^. #errors of
      [] -> pure $ res
      errors -> Left $ errors

  prettyLines :: [T.Text]
  prettyLines = case typedAst of
    Left nasra -> nasra
    Right good -> prettyPrintSProgram sourceFile good
