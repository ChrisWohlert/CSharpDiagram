module PathFinder ( buildPath
                  , Pair( .. )
                  ) where 

import Data.Maybe
import Data.List
import qualified Data.Map as M
import qualified Debug.Trace as D
import Data.List.Extra

newtype Pair = Pair { getPair :: (Double, Double) } deriving (Show, Eq, Ord)

class Navigable a where
    east :: a -> Maybe a
    south :: a -> Maybe a
    west :: a -> Maybe a
    north :: a -> Maybe a
    northeast :: a -> Maybe a
    southeast :: a -> Maybe a
    southwest :: a -> Maybe a
    northwest :: a -> Maybe a     

instance Navigable Pair where
    east  (Pair (x, y)) = Just $ Pair (x + 1, y)
    south (Pair (x, y)) = Just $ Pair (x, y + 1)
    west (Pair (x, y)) = Just $ Pair (x - 1, y)
    north (Pair (x, y)) = Just $ Pair (x, y - 1)
    northeast (Pair (x, y)) = Just $ Pair (x + 1, y - 1)
    southeast (Pair (x, y)) = Just $ Pair (x + 1, y + 1)
    southwest (Pair (x, y)) = Just $ Pair (x - 1, y + 1)
    northwest (Pair (x, y)) = Just $ Pair (x - 1, y - 1)

type Cost = Double

data Visited = Visited | NotVisited deriving (Eq)

buildPath root (Pair(endX, endY)) cost = buildP (M.singleton root (0, root, Visited)) root
    where
        buildP nodes node@(Pair(x, y))
            | abs (x - endX) < 1 && abs (y - endY) < 1 = getPathFrom nodes node
            | otherwise = 
                let
                    nexts = catMaybes $ map ($ node) [east, south, west, north, northeast, southeast, southwest, northwest]
                    updatedNodes = foldl (\ updated next -> M.insertWith chooseCheapest next (cost next + (getCost nodes node), node, NotVisited) updated) nodes nexts
                    markAsVisited = M.update (\ (c, a, v) -> Just (c, a, Visited)) node updatedNodes
                    nextNode = (getNextNode markAsVisited)
                in
                    buildP markAsVisited (D.trace (show nextNode) nextNode) 
        chooseCheapest (c1, a1, v1) (c2, a2, v2) = if c1 < c2 then (c1, a1, v1) else (c2, a2, v2)
        getCost nodes node 
            | M.notMember node nodes = 0
            | otherwise = 
                let
                    (c, a, v) = nodes M.! node
                in
                    c
        getPathFrom nodes node = 
            let
                (c, a, v) = nodes M.! node
            in
                if node == a then [node] else node : (getPathFrom nodes a)
        getNextNode nodes = fst $ minimumOn (\ (k, (c, a, v)) -> c) $ filter (\ (k, (c, a, v)) -> v == NotVisited) $ M.toList nodes