{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use section" #-}
module Utils.Algorithms (
  dagToTree,
  subtype,
  lca,
) where

import Control.Lens.Getter ((^.))
import Data.Generics.Labels ()
import Data.List ()
import Data.List.NonEmpty (NonEmpty (..))
import Data.List.NonEmpty.Extra qualified as NE (fromList, head)
import Data.Map.NonEmpty qualified as NEM (toList, (!?))
import Data.Maybe (fromMaybe)
import Data.Tuple.Extra (both)
import Text.Printf (printf)
import Typist.Types (
  Graph,
  Path,
  Tree (Tree),
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
