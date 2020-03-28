module PathNavigator where



data Collidable = Collidable { collidePos :: (Double, Double), collideWidth :: Double, collideHeight :: Double }

data StepCollision = StepCollision Collidable

type StepPosition = (Double, Double)


data Step = Step { stepPos :: StepPosition
                 , stepVector :: (Double, Double)
                 , nextStep :: Step
                 , stepCollision :: Maybe StepCollision
                 }

filterCollidableToProblemSpace :: StepPosition -> StepPosition -> [Collidable] -> [Collidable]
filterCollidableToProblemSpace (startX, startY) (endX, endY) cs = filter isInside cs
    where
        isInside (Collidable (x, y) w h) = lowestX < x && highestY > y && highestX > x && lowestY < y
        lowestX = minimum [startX, endX]
        lowestY = minimum [startY, endY]
        highestX = maximum [startX, endX]
        highestY = maximum [startY, endY]

getCollisions :: 