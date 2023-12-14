module Utils.Algorithms (detectCycle) where

import Control.Applicative (asum)
import Control.Lens (makeLenses, (%~), (&))
import Control.Lens.Getter ((^.))

type Node = Int
type Graph = [(Node, [Node])]

data Info = Info
  { _seen :: [Node]
  , _path :: [Node]
  }

makeLenses ''Info

-- >>> detectCycle [(1, [3]), (2, [3]), (3, [2])]
-- Just [3,2]

-- | Detect cycles in a graph (has to have only one connected component)
detectCycle :: Graph -> Maybe [Node]
detectCycle [] = Nothing
detectCycle graph@((start, _) : _) = relaxNode start (Info [] [])
 where
  relaxNode :: Node -> Info -> Maybe [Node]
  relaxNode curr info =
    if
        -- Found a cycle
        | curr `elem` (info ^. seen) ->
            Just $ dropWhile (curr /=) (reverse $ info ^. path)
        -- Still room to go
        | not $ null edges ->
            asum $ map (\x -> relaxNode x $ info & seen %~ (curr :) & path %~ (curr :)) edges
        -- Done
        | otherwise ->
            Nothing
   where
    edges :: [Node]
    edges = findNeighbours graph curr

  findNeighbours :: Graph -> Node -> [Node]
  findNeighbours [] _ = []
  findNeighbours ((v, edges) : rest) seek =
    if v == seek then edges else findNeighbours rest seek
