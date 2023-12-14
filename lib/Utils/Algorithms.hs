module Utils.Algorithms(detectCycle) where

import Control.Applicative (asum)
import Control.Lens (makeLenses, (&), (%~))
import Control.Lens.Getter ((^.))

type Node = Int
type Graph = [(Node, [Node])]

data Info = Info {
  _seen :: [Node],
  _path :: [Node]
}

makeLenses ''Info

detectCycle :: Graph -> Maybe [Node]
detectCycle [] = Nothing
detectCycle graph@((start, _) : _) = relaxNode start (Info [] [])
  where

    relaxNode :: Node -> Info -> Maybe [Node]
    relaxNode curr info = let edges = findNeighbours graph curr in
      if curr `notElem` (info ^. seen) then
        if null edges
          then Nothing
          else asum $ map (\x -> relaxNode x $ info & seen %~ (curr :) & path %~ (curr :)) edges
      else
        Just $ dropWhile (curr /=) (reverse $ info ^. path)

    findNeighbours :: Graph -> Node -> [Node]
    findNeighbours [] _ = []
    findNeighbours ((v, edges) : rest) seek =
      if v == seek then edges else findNeighbours rest seek

-- >>> detectCycle [(1, [2]), (2, [3]), (3, [2])]

