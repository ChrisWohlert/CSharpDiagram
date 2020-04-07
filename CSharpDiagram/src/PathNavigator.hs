module PathNavigator ( filterCollidableToProblemSpace
                     , getPath
                     , Collidable(..)) where

import TwoDVector
import Data.Maybe
import Data.List
import qualified Debug.Trace as D


data Collidable = Collidable { collidePos :: (Double, Double), collideWidth :: Double, collideHeight :: Double }

newtype CollidableInProblemSpace = CollidableInProblemSpace { getCollidable :: Collidable }

data Collision = Collision CollidableInProblemSpace Point


getPath :: Point -> Point -> [CollidableInProblemSpace] -> [Point]
getPath from to collidables = reverse $ sortOn (\ (Point (x, y)) -> x) $ from : (map (\ (Collision _ x) -> x) getCollisions) ++ [to]
    where
        direction = getCardinalDirection $ v pv
        pv = from .-. to
        getCollidableVectors = case direction of
                                    North     -> [bottom]
                                    East      -> [left]
                                    South     -> [top]
                                    West      -> [right]
                                    NorthEast -> [bottom, left]
                                    SouthEast -> [top, left]
                                    SouthWest -> [top, right]
                                    NorthWest -> [bottom, right]
                                    Nowhere -> []
        moveCollisions (Collision (CollidableInProblemSpace (Collidable (x, y) w h)) (Point (px, py)))
            | x == px && y - py < h = (Point (x, y + 3))
            | x == px && y - py > h = (Point (x, y - h - 3))
            | x + w == px && y - py < h = (Point (x + w, y + 3))
            | x + w == px && y - py > h = (Point (x + w, y - h - 3))
            | y == py && px - x < w = (Point (x - 3, y))
            | y == py && px - x > w = (Point (x + w + 3, y))
            | y - h == py && px - x < w = (Point (x - 3, y - h))
            | y - h == py && px - x > w = (Point (x + w + 3, y - h))
            | otherwise = (Point (px, py))
        getCollisions = [Collision c x | f <- getCollidableVectors, c <- collidables, let straightV = makeStraightVector (f c), let x = getVectorIntersection pv (fromJust straightV), isCollision c (D.trace (show (c, x)) x), isJust straightV]
        isCollision (CollidableInProblemSpace (Collidable (x, y) w h)) (Point (px, py)) = --True
            (round x == round px && round py < round y && round py > round y - round h) ||
            (round x + round w == round px && round py < round y && round py > round y - round h) ||
            (round y == round py && round px > round x && round px < round x + round h) ||
            (round y - round h == round py && round px > round x && round px < round x + round h)
        bottom (CollidableInProblemSpace (Collidable (x, y) w h)) = mkPV (x, y - h) (1, 0)
        left (CollidableInProblemSpace (Collidable (x, y) w h)) = mkPV (x, y) (0, -1)
        top (CollidableInProblemSpace (Collidable (x, y) w h)) = mkPV (x, y) (1, 0)
        right (CollidableInProblemSpace (Collidable (x, y) w h)) = mkPV (x + w, y) (0, -1)

filterCollidableToProblemSpace :: Point -> Point -> [Collidable] -> [CollidableInProblemSpace]
filterCollidableToProblemSpace (Point (startX, startY)) (Point (endX, endY)) = map CollidableInProblemSpace . filter isInside
    where
        isInside (Collidable (x, y) w h) = lowestX < x && highestY > y && highestX > x && lowestY < y
        lowestX = minimum [startX, endX]
        lowestY = minimum [startY, endY]
        highestX = maximum [startX, endX]
        highestY = maximum [startY, endY]