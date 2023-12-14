module Typist.Types (
  -- Algorithms
  type Graph,
  type Path,
  Tree (..),
  -- lens
  node,
  neighbours,
  -- Types
  type Type,
  type Class,
  type Identifier,
  type O,
  type M,
  type C,
  Context (..),
  -- lens
  variableTypes,
  methodTypes,
  currentClass,
  classHierarchy,
  featureTypes,
) where

import Control.Lens (makeLenses)
import Data.List.NonEmpty (NonEmpty)
import Data.Map (Map)
import Data.Map.NonEmpty (NEMap)
import Data.Text qualified as T

type Graph a = NEMap a [a]
type Path a = [a]

data Tree a = Tree
  { _node :: a
  , _neighbours :: [Tree a]
  }
  deriving stock (Show)

makeLenses ''Tree

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
  { _variableTypes :: O
  , _featureTypes :: O
  , _methodTypes :: M
  , _currentClass :: C
  , _classHierarchy :: Tree Type
  }

makeLenses ''Context
