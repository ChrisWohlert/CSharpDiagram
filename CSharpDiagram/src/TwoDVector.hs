module TwoDVector ( Point(..)
                  , Vector(..)
                  , PointVector(..)
                  , (.-.)
                  , CardinalDirection(..)
                  , getCardinalDirection
                  , v
                  , mkPV
                  , getVectorIntersection
                  , makeStraightVector) where

data Point = Point (Double, Double) deriving (Show)

data Vector = Vector (Double, Double)

data PointVector = PointVector Point Vector

data CardinalDirection = North | East | South | West | NorthEast | SouthEast | SouthWest | NorthWest | Nowhere

data StraightVector = VerticalVector PointVector | HorizontalVector PointVector

(.-.) :: Point -> Point -> PointVector
(.-.) p@(Point (x1, y1)) (Point (x2, y2)) = PointVector p (Vector (x2 - x1, y2 - y1))

v (PointVector p vec) = vec

getVectorIntersection :: PointVector -> StraightVector -> Point 
getVectorIntersection (PointVector (Point (x1, y1)) (Vector (vx1, vy1))) (VerticalVector (PointVector (Point (x2, y2)) (Vector (vx2, vy2)))) =
    let
        s = (x2 - x1) / vx1
    in
        Point (x1 + vx1 * s, y1 + vy1 * s)
getVectorIntersection (PointVector (Point (x1, y1)) (Vector (vx1, vy1))) (HorizontalVector (PointVector (Point (x2, y2)) (Vector (vx2, vy2)))) =
    let
        s = (y2 - y1) / vy1
    in
        Point (x1 + vx1 * s, y1 + vy1 * s)

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