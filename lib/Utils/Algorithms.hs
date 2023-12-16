{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
{-# HLINT ignore "Avoid lambda" #-}
module Utils.Algorithms (
  dagToTree,
  subtype,
  lca,
  extendO,
  classHierarchyTree,
  classHierarchyGraph,
  allClassesWithParents,
  findFirstDuplicate,
) where

import Control.Arrow (Arrow (..))
import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.List ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty qualified as NE (filter, toList, zip)
import Data.List.NonEmpty.Extra qualified as NE (fromList, head)
import Data.Map qualified as M (empty, fromList, union)
import Data.Map.NonEmpty (NEMap)
import Data.Map.NonEmpty qualified as NEM (
  fromList,
  insertWith,
  singleton,
  toList,
  (!?),
 )
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Tuple.Extra (both)
import Parser.Types (
  ExtraInfo (..),
  SBinding (..),
  SClass (..),
  SFeature (..),
  SProgram (..),
 )
import Text.Printf (printf)
import Typist.Types (
  Class,
  Graph,
  Identifier,
  O,
  Path,
  Tree (Tree),
  Type,
 )

-- >>> dagToTree $ NEM.fromList $ NE.fromList [("1", ["3"]), ("2", ["3"]), ("3", ["4", "5"]), ("4", []), ("5", ["5"])]
-- Left ["5"]

-- | Detect cycles in a graph (has to have only one connected component)
dagToTree :: forall a. (Ord a, Show a) => Graph a -> Either (NonEmpty a) (Tree a)
dagToTree graph = relaxNode [] start
 where
  start :: a
  start = fst $ NE.head $ NEM.toList graph

  relaxNode :: Path a -> a -> Either (NonEmpty a) (Tree a)
  relaxNode path curr =
    if
        -- Found a cycle
        | curr `elem` path ->
            Left $ NE.fromList $ dropWhile (/= curr) $ reverse path
        -- Still room to go
        | not $ null edges ->
            Tree curr <$> traverse (relaxNode (curr : path)) edges
        -- Leaf, return 'singleton' tree
        | otherwise ->
            Right $ Tree curr []
   where
    edges :: [a]
    edges =
      fromMaybe
        (error $ printf "Incomplete tree (undefined node %s)" (show curr))
        (graph NEM.!? curr)

-- >>> lca (Tree 1 [Tree 2 [], Tree 3 [Tree 4 [], Tree 5 []]]) 4 5
-- 3

commonPrefix :: (Eq a) => ([a], [a]) -> [a]
commonPrefix (l1, l2) = fst <$> takeWhile (uncurry (==)) (zip l1 l2)

lca :: forall a. (Eq a) => Tree a -> a -> a -> a
lca tree x y = last $ commonPrefix $ both (explore [] tree) (x, y)
 where
  explore :: Path a -> Tree a -> a -> Path a
  explore ans tree seek =
    if seek == tree ^. #node
      then reverse $ tree ^. #node : ans
      else tree ^. #neighbours >>= (\subtree -> explore ((tree ^. #node) : ans) subtree seek)

-- >>> subtype (Tree 1 [Tree 2 [], Tree 3 [Tree 4 [], Tree 5 []]]) 1 1
-- True

subtype :: (Eq a) => Tree a -> a -> a -> Bool
subtype tree x y = lca tree x y == y

extendO :: NonEmpty (SClass ExtraInfo) -> NEMap Class Class -> NEMap Class O
extendO classes dg = NEM.fromList $ ((.name) *** handler) <$> NE.zip classes inheritancePaths
 where
  inheritancePaths :: NonEmpty [Class]
  inheritancePaths = reverse . goUp . (.name) <$> classes

  -- [Object, Shape, Rectangle, Square]
  -- Ractangle { x: Char }
  -- Square { x: Int, y : Char }

  -- TODO: Separate namespaces between class functions and class fields
  handler :: [Class] -> O
  handler classes' =
    let features = (M.fromList . (f <$>) . (.features) <$> NE.filter (\x -> x.name `elem` classes') classes)
     in foldr (\acc curr -> acc `M.union` curr) M.empty features

  f :: SFeature ExtraInfo -> (Identifier, Type)
  f = \case
    SFeatureMember{fbinding = SBinding{bidentifier, btype}} -> (bidentifier, btype)
    SFeatureMethod{fidentifier, ftype} -> (fidentifier, ftype)

  goUp :: Class -> [Class]
  goUp name = case dg NEM.!? name of
    Just parent -> name : goUp parent
    Nothing -> [name]

allClassesWithParents :: NonEmpty (SProgram ExtraInfo) -> Either String (NEMap Class Class)
allClassesWithParents programs =
  let classesList = programs >>= (.pclasses)
      sclassToclassWithParent (SClass{name, parent}) = (name, fromMaybe "Object" parent)
      classesWithParentsList = sclassToclassWithParent <$> classesList
      classesWithParents = NEM.fromList classesWithParentsList
   in maybe
        (Right classesWithParents)
        (Left . printf "Class %s is defined more than once")
        (findFirstDuplicate $ (.name) <$> NE.toList classesList)

findFirstDuplicate :: (Ord a) => [a] -> Maybe a
findFirstDuplicate = go Set.empty
 where
  go :: (Ord a) => Set a -> [a] -> Maybe a
  go _ [] = Nothing
  go seen (x : xs) =
    if Set.member x seen
      then Just x
      else go (Set.insert x seen) xs

classHierarchyGraph :: NEMap Class Class -> NEMap Class (Set Class)
classHierarchyGraph classesWithParents =
  foldl
    ( \hierarchy (name, parent) ->
        NEM.insertWith
          Set.union
          parent
          (Set.singleton name)
          hierarchy
    )
    (NEM.singleton "Object" Set.empty)
    (NEM.toList classesWithParents)

classHierarchyTree :: NEMap Class (Set Class) -> Either (NonEmpty Class) (Tree Class)
classHierarchyTree = dagToTree . (Set.toList <$>)
