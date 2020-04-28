module PathNavigator ( filterCollidableToProblemSpace
                     , getPath
                     , getPath'
                     , getPoints
                     , Collidable(..)
                     , getCollisions
                     , getPointOfCollision) where

import TwoDVector
import Data.Maybe
import Data.List
import Safe
import Data.Ord
import qualified Debug.Trace as D


data Collidable = Collidable { collidePos :: (Double, Double), collideWidth :: Double, collideHeight :: Double } deriving (Show, Eq)

newtype CollidableInProblemSpace = CollidableInProblemSpace { getCollidable :: Collidable } deriving (Show, Eq)

data Collision = Collision CollidableInProblemSpace Point deriving (Show, Eq)

data Path = Line Point Point | SubPaths [Path]

getPoints (SubPaths []) = []
getPoints (Line p p2) = [p, p2]
getPoints (SubPaths points) = concatMap getPoints points


getPath' :: [CollidableInProblemSpace] -> Point -> Point -> Path
getPath' collidables from to
    | null (getCollisions collidables from to) = Line from to
    | otherwise = let ([a, b]:cs) = map moveCollisions (getCollisions collidables from to) in SubPaths [getPath' collidables from a, getPath' collidables b to]

getPath :: Point -> Point -> [CollidableInProblemSpace] -> [Point]
getPath from to collidables = concat $ unfoldr takeWhileNotColliding sortedPath
    where
        takeWhileNotColliding [] = Nothing
        takeWhileNotColliding [p] = Just ([p], [])
        takeWhileNotColliding [p, p2] = Just ([p], [p2])
        takeWhileNotColliding (p:ps) = 
            let
                next = findNextNode p ps
                rest = if not $ null next then tailSafe $ dropWhile (/= last next) ps else ps
            in
                Just (p : next, rest)
        findNextNode p ps = case headMay $ dropWhile (not . null . getCollisions collidables p) (sortOn (pointsDistance to) ps) of
                                Just e -> [e]
                                Nothing -> init $ tail $ getPath p (head ps) collidables
        sortedPath = sortOn (Down . pointsDistance to) $ from : concatMap moveCollisions (getCollisions collidables from to ) ++ [to]

write x = D.trace (show x) x
name l x = D.trace (show l ++ ": " ++ show x) x

getCollisions collidables p1 p2 = 
    [Collision c (fromJust x) | 
    f <- getCollidableVectors p1 p2, 
    c <- collidables,
    let straightV = makeStraightVector (f c), 
    let x = getVectorIntersection (p1 .-. p2) (fromJust straightV), 
    isCollision c (fromJust x), 
    isJust straightV, 
    isJust x]

getPointOfCollision (Collision _ point) = point

direction p1 p2 = getCardinalDirection $ v (p1 .-. p2)

isCollision (CollidableInProblemSpace (Collidable (x, y) w h)) (Point (px, py)) = x <= px && x + w >= px && y >= py && y - h <= py

moveCollisions (Collision (CollidableInProblemSpace (Collidable (x, y) w h)) (Point (px, py)))
    | abs (x - px)     <= 0.01 && y - py <= h / 2 = [(Point (x, y + 4)), (Point (x + w, y + 4))]
    | abs (x - px)     <= 0.01 && y - py >  h / 2 = [(Point (x, y - h - 4)), (Point (x + w, y - h - 4))]
    | abs (x + w - px) <= 0.01 && y - py <= h / 2 = [(Point (x + w, y + 4)), (Point (x, y + 4))]
    | abs (x + w - px) <= 0.01 && y - py >  h / 2 = [(Point (x + w, y - h - 4)), (Point (x, y - h - 4))]
    | abs (y - py)     <= 0.01 && px - x <= w / 2 = [(Point (x - 4, y)), (Point (x - 4, y - h))]
    | abs (y - py)     <= 0.01 && px - x >  w / 2 = [(Point (x + w + 4, y)), (Point (x + w + 4, y - h))]
    | abs (y - h - py) <= 0.01 && px - x <= w / 2 = [(Point (x - 4, y - h)), (Point (x - 4, y))]
    | abs (y - h - py) <= 0.01 && px - x >  w / 2 = [(Point (x + w + 4, y - h)), (Point (x + w + 4, y))]
    | otherwise = error "This isn't a collision"

getCollidableVectors p1 p2 = case direction p1 p2 of
                            North     -> [bottom]
                            East      -> [left]
                            South     -> [top]
                            West      -> [right]
                            NorthEast -> [bottom, left]
                            SouthEast -> [top, left]
                            SouthWest -> [top, right]
                            NorthWest -> [bottom, right]
                            Nowhere -> []

bottom (CollidableInProblemSpace (Collidable (x, y) w h)) = mkPV (x, y - h) (1, 0)
left (CollidableInProblemSpace (Collidable (x, y) w h)) = mkPV (x, y) (0, -1)
top (CollidableInProblemSpace (Collidable (x, y) w h)) = mkPV (x, y) (1, 0)
right (CollidableInProblemSpace (Collidable (x, y) w h)) = mkPV (x + w, y) (0, -1)

filterCollidableToProblemSpace :: Point -> Point -> [Collidable] -> [CollidableInProblemSpace]
filterCollidableToProblemSpace (Point (startX, startY)) (Point (endX, endY)) = map CollidableInProblemSpace . filter isInside
    where
        isInside (Collidable (x, y) w h) = lowestX < x + w && highestY > y - h && highestX > x && lowestY < y
        lowestX = minimum [startX, endX]
        lowestY = minimum [startY, endY]
        highestX = maximum [startX, endX]
        highestY = maximum [startY, endY]