{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use ++" #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Redundant <$>" #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Utils.Pretty.Typist (
  lexParseTypeAndPrettyPrint,
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
import Typist.Types (
  Class,
  Context (..),
  Identifier,
  O,
  Tree,
  Type,
 )
import Utils.Pretty.Parser (
  prettyPrintMaybeSExpr,
  prettyPrintSExpr,
  prettyPrintSFeature,
  prettyPrintSFormal,
  prettyPrintSProgram,
  prettyprintSClass,
 )
import Utils.Algorithms (
  allClassesWithParents,
  classHierarchyGraph,
  classHierarchyTree,
  dagToTree,
  extendO,
  findFirstDuplicate,
  lca,
  subtype,
 )
import qualified Data.Map as Map

lexParseTypeAndPrettyPrint :: (FilePath, T.Text) -> T.Text
lexParseTypeAndPrettyPrint (sourceFile, sourceCode) =
  T.unlines prettyLines
 where
  lexemes :: [Lexeme]
  lexemes = lexer sourceCode

  ast :: SProgram ExtraInfo
  ast = fromRight' $ parse parser sourceFile lexemes

  initialContext :: Either String Context
  initialContext = do
    let programs = pure ast
    classParentHirearchy <- allClassesWithParents programs
    let identifierTypes = Map.empty

    -- TODO: calculate (in Algorithms)
    let methodTypes = undefined
    let currentClass = ""
    let classHierarchy = undefined

    pure $
      Context
        { identifierTypes
        , methodTypes
        , currentClass
        , classHierarchy
        , programs
        , classParentHirearchy
        }

  typedAst :: Either String (SProgram ExtraInfo)
  typedAst = undefined ast initialContext

  prettyLines :: [T.Text]
  prettyLines = prettyPrintSProgram sourceFile ast
