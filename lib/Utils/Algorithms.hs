{-# OPTIONS_GHC -Wno-unused-top-binds #-}
module Utils.Algorithms (detectCycle) where

import Control.Lens (makeLenses, (%~), (&))
import Control.Lens.Getter ((^.))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.List.NonEmpty.Extra (fromList)
import Control.Applicative (asum)
import Data.List ()
import Data.Tuple.Extra (both)

type Graph a = NonEmpty (a, [a])
type Path a = [a]

data Info a = Info
  { _seen :: [a]
  , _path :: Path a
  }

data Tree a where
  Tree :: a -> [Tree a] -> Tree a
  deriving stock (Show)

makeLenses ''Info

-- >>> detectCycle [(1, [3]), (2, [3]), (3, [2])]
-- Just [3,2]

-- | Detect cycles in a graph (has to have only one connected component)
detectCycle :: forall a. (Eq a) => Graph a -> Either [a] (Tree a)
detectCycle graph@((start, _) :| _) = relaxNode start (Info [] [])
 where
  relaxNode :: a -> Info a -> Either [a] (Tree a)
  relaxNode curr info =
    if
        -- Found a cycle
        | curr `elem` (info ^. seen) ->
            Left $ dropWhile (curr /=) (reverse $ info ^. path)
        -- Still room to go
        | not $ null edges -> Tree curr <$> traverse (\x -> relaxNode x $ info & seen %~ (curr :) & path %~ (curr :)) edges
        -- Leaf, return 'singleton' tree
        | otherwise ->
            Right $ Tree curr []
   where
    edges :: [a]
    edges = findNeighbours graph curr

  findNeighbours :: Graph a -> a -> [a]
  findNeighbours ((v, edges) :| rest) seek =
    if v == seek then edges else findNeighbours (fromList rest) seek

-- >>> lca (Tree 1 [Tree 2 [], Tree 3 []]) 2 3

intersect :: (Eq a) => ([a], [a]) -> [a]
intersect (l1, l2) = fst <$> takeWhile (uncurry (==)) (zip l1 l2)

lca :: forall a. (Eq a) => Tree a -> a -> a -> a
lca tree x y = last $ intersect $ both (explore tree []) (x, y)

explore :: (Eq a) => Tree a -> Path a -> a -> Path a
-- Leaf
explore (Tree curr []) ans seek = if seek == curr then reverse $ curr : ans else []
-- Non-Leaf
explore (Tree curr rest) ans seek = if seek == curr then reverse $ curr : ans else
  concatMap (\x -> explore x (curr : ans) seek) rest

