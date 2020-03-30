module PathNavigator where



data Collidable = Collidable { collidePos :: (Double, Double), collideWidth :: Double, collideHeight :: Double }

newtype CollidableInProblemSpace = CollidableInProblemSpace { getCollidable :: Collidable }

type StepPosition = (Double, Double)
type StepVector = (Double, Double)
type StepPointVector = (StepPosition, StepVector)


getPath :: StepPosition -> StepPosition -> [CollidableInProblemSpace] -> [StepPosition]
getPath = undefined

filterCollidableToProblemSpace :: StepPosition -> StepPosition -> [Collidable] -> [CollidableInProblemSpace]
filterCollidableToProblemSpace (startX, startY) (endX, endY) = map CollidableInProblemSpace . filter isInside
    where
        isInside (Collidable (x, y) w h) = lowestX < x && highestY > y && highestX > x && lowestY < y
        lowestX = minimum [startX, endX]
        lowestY = minimum [startY, endY]
        highestX = maximum [startX, endX]
        highestY = maximum [startY, endY]

getVectorIntersection :: StepPointVector -> StepPointVector -> StepPosition 
getVectorIntersection ((x1, y1), (vx1, vy1)) ((x2, y2), (vx2, vy2)) =
    let
        c = vy2 / vx2
        s = ((y2 - y1) - ((x2 - x1) * c)) / (vy1 - (vx1 * c))
    in
        (x1 + vx1 * s, y1 + vy1 * s)