module Typist.Types (
  -- Algorithms
  type Graph,
  type Path,
  Tree (..),
  -- Types
  type Type,
  type Class,
  type Identifier,
  type O,
  type M,
  type C,
  Context (..),
) where

import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map.NonEmpty (NEMap)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Parser.Types (ExtraInfo, SProgram)

type Graph a = NEMap a [a]
type Path a = [a]

data Tree a = Tree
  { node :: a
  , neighbours :: [Tree a]
  }
  deriving stock (Generic, Show)

type Type = T.Text
type Class = T.Text
type Identifier = T.Text

-- Variable -> Type
type O = Map Identifier Type

-- Class + function name -> list of argument types (+ return value)
type M = Map (Type, Identifier) (NonEmpty Type)

-- Current class
type C = Class

data Context = Context
  { identifierTypes :: O
  , methodTypes :: M
  , currentClass :: C
  , classHierarchy :: Tree Type
  , programs :: NonEmpty (SProgram ExtraInfo)
  , classParentHirearchy :: NEMap Class Class
  , errors :: [T.Text]
  }
  deriving stock (Generic)
