module TwoDVector ( Point(..)
                  , Vector(..)
                  , PointVector(..)
                  , (.-.)
                  , CardinalDirection(..)
                  , getCardinalDirection
                  , v
                  , mkPV
                  , getVectorIntersection
                  , makeStraightVector
                  , pointsDistance
                  , StraightVector(..)) where

data Point = Point (Double, Double) deriving (Show, Eq)

data Vector = Vector (Double, Double)deriving (Show, Eq)

data PointVector = PointVector Point Vector deriving (Show, Eq)

data CardinalDirection = North | East | South | West | NorthEast | SouthEast | SouthWest | NorthWest | Nowhere deriving (Show, Eq)

data StraightVector = VerticalVector PointVector | HorizontalVector PointVector deriving (Show, Eq)

(.-.) :: Point -> Point -> PointVector
(.-.) p@(Point (x1, y1)) (Point (x2, y2)) = PointVector p (Vector (x2 - x1, y2 - y1))

v (PointVector p vec) = vec

getVectorIntersection :: PointVector -> StraightVector -> Maybe Point 
getVectorIntersection p1@(PointVector (Point (x1, y1)) (Vector (vx1, vy1))) (VerticalVector p2@(PointVector (Point (x2, y2)) (Vector (vx2, vy2))))
    | p1 == p2 = Nothing
    | vx1 == 0 = Nothing
    | otherwise = Just (
        let
            s = (x2 - x1) / vx1
        in
            Point (x1 + vx1 * s, y1 + vy1 * s))
getVectorIntersection (PointVector p1@(Point (x1, y1)) (Vector (vx1, vy1))) (HorizontalVector (PointVector p2@(Point (x2, y2)) (Vector (vx2, vy2))))
    | p1 == p2 = Nothing
    | vy1 == 0 = Nothing
    | otherwise = Just (
        let
            s = (y2 - y1) / vy1
        in
            Point (x1 + vx1 * s, y1 + vy1 * s))

makeStraightVector pv@(PointVector p (Vector (0, y))) = Just $ VerticalVector pv
makeStraightVector pv@(PointVector p (Vector (x, 0))) = Just $ HorizontalVector pv
makeStraightVector _ = Nothing

mkPV p v = PointVector (Point p) (Vector v)

getCardinalDirection (Vector (x, y))
    | x == 0 && y > 0 = North
    | x == 0 && y < 0 = South
    | x > 0 && y == 0 = East
    | x < 0 && y == 0 = West
    | x > 0 && y > 0 = NorthEast
    | x > 0 && y < 0 = SouthEast
    | x < 0 && y > 0 = NorthWest
    | x < 0 && y < 0 = SouthWest
    | otherwise = Nowhere
    

pointsDistance (Point (x1, y1)) (Point (x2, y2)) = sqrt $ (x2 - x1) ** 2 + (y2 - y1) ** 2