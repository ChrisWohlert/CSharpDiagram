{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}
{-# LANGUAGE FlexibleInstances         #-}

module PathNavigatorTest where

import Test.Hspec
import PathNavigator
import TwoDVector
import Data.List
import Test.QuickCheck
import Debug.Trace
import Arbitraries
import qualified Diagrams.Prelude as D
import qualified Diagrams.Backend.SVG as SVG

testPathNavigator = do 
    describe "PathNavigator.filterCollidableToProblemSpace" $
        it "returns the collidables inside two points" $
        length (filterCollidableToProblemSpace (Point (0, 0)) (Point (10, 10)) [(Collidable (1, 1) 2 2), (Collidable (1, -1) 2 2)]) `shouldBe` 1
        
    describe "PathNavigator.filterCollidableToProblemSpace" $
        it "returns the collidables inside two points" $
        length (filterCollidableToProblemSpace (Point (0, 0)) (Point (10, 10)) [(Collidable (1, 1) 2 2), (Collidable (9, 11) 2 2)]) `shouldBe` 2
        
    describe "PathNavigator.filterCollidableToProblemSpace" $
        it "returns the collidables inside two points" $
        length (filterCollidableToProblemSpace (Point (10, -10)) (Point (0, 0)) [(Collidable (1, 1) 2 2), (Collidable (9, -9) 2 2)]) `shouldBe` 2
        
    describe "PathNavigator.getPath" $
        it "returns the path between two points" $
        getPath (Point (3, 12)) (Point (13, 2)) (filterCollidableToProblemSpace (Point (3, 12)) (Point (13, 2)) [(Collidable (5, 11) 2 4), (Collidable (9, 9) 2 4)]) 
        `shouldBe` 
        [(Point (3, 12)), (Point (7, 12)), (Point (9, 4)), (Point (13, 2))]
        
    describe "PathNavigator.getPath" $
        it "returns the path between two points going up and left" $
        getPath (Point (14, 2)) (Point (4, 12)) (filterCollidableToProblemSpace (Point (14, 2)) (Point (4, 12)) [(Collidable (6, 9) 2 4), (Collidable (10, 7) 2 4)]) 
        `shouldBe` 
        [(Point (14, 2)), (Point (10, 2)), Point (8.0,10.0),Point (4, 12)]
        
    describe "PathNavigator.getPath" $
        xit "returns the path between two points going up and left" $
        getPath (Point (13, 2)) (Point (2, 9)) (filterCollidableToProblemSpace (Point (13, 2)) (Point (2, 9)) [(Collidable (3, 10) 2 4), (Collidable (9, 10) 2 7)]) 
        `shouldBe` 
        [(Point (13, 2)), (Point (9, 2)), (Point (3, 5)), (Point (2, 9))]
        
    describe "PathNavigator.getPath" $
        it "returns the path between two points going up and left through bottom of collidable" $
        getPath (Point (9, 1)) (Point (4, 11)) (filterCollidableToProblemSpace (Point (9, 1)) (Point (4, 11)) [(Collidable (6, 10) 2 5)]) 
        `shouldBe` 
        [(Point (9, 1)), (Point (5, 5)), (Point (4, 11))]
        
    describe "PathNavigator.getPath" $
        it "returns the path between two points going up through bottom of collidable" $
        getPath (Point (9, 2)) (Point (9, 13)) (filterCollidableToProblemSpace (Point (9, 2)) (Point (9, 13)) [(Collidable (8, 9) 2 4), (Collidable (8, 12) 2 2)])
        `shouldBe` 
        [(Point (9, 2)), (Point (7, 5)), (Point (7, 12)), (Point (9, 13))]
        
    describe "PathNavigator.getPath" $
        it "returns the path between two points going up and left from negative" $
        getPath (Point (12, -13)) (Point (2, -4)) (filterCollidableToProblemSpace (Point (12, -13)) (Point (2, -4)) [(Collidable (5, -6) 2 4), (Collidable (8, -10) 2 6)])
        `shouldBe` 
        [Point (12.0,-13.0),Point (10.0,-9.0),Point (5.0,-11.0),Point (2.0,-4.0)]
        
    describe "PathNavigator.getPath" $
        it "returns the path between two points, taking the quickest route" $
        getPath (Point (13, 2)) (Point (2, 9)) (filterCollidableToProblemSpace (Point (13, 2)) (Point (2, 9)) [(Collidable (9, 7) 2 3), (Collidable (5, 11) 2 8)])
        `shouldBe` 
        [(Point (13, 2)), (Point (5, 2)), (Point (2, 9))]
        
    describe "PathNavigator.getPath" $
        xit "returns the path between two points, taking the quickest route" $
        getPoints (getPath' (filterCollidableToProblemSpace (Point (2, 12)) (Point (13, 10)) [(Collidable (5, 11) 2 4), (Collidable (9, 15) 2 6)]) (Point (2, 12)) (Point (13, 10)))
        `shouldBe` 
        [(Point (2, 12)), (Point (7, 12)), (Point (9, 8)), (Point (11, 8)), (Point (13, 10))]
        
    describe "PathNavigator.getPath" $
        it "returns the path between two points, with no collisions" $
        getPath (Point (1, 1)) (Point (5, 5)) (filterCollidableToProblemSpace (Point (1, 1)) (Point (5, 5)) [])
        `shouldBe` 
        [(Point (1, 1)), (Point (5, 5))]
        
    describe "PathNavigator.getPath" $
        it "returns the path between two points, with no collisions" $
        getPath (Point (1, 1)) (Point (5, -5)) (filterCollidableToProblemSpace (Point (1, 1)) (Point (5, -5)) [])
        `shouldBe` 
        [Point (1, 1), Point (5, -5)]
        
    describe "PathNavigator.getPath" $
        xit "returns the path between two points, with no collisions" $
        getPath (Point (4.77730872434096,-0.48832774171579896)) (Point (6.2253398785972855,1.3985819196858422)) (filterCollidableToProblemSpace (Point (4.77730872434096,-0.48832774171579896)) (Point (6.2253398785972855,1.3985819196858422)) [Collidable {collidePos = (4.77730872434096,1.8940220377564376), collideWidth = 2.0, collideHeight = 1.3823497794722366}])
        `shouldBe` 
        [(Point (4.77730872434096,-0.48832774171579896)), (Point (6.2253398785972855,1.3985819196858422))]

testPathNavigatorProperties = do
    _ <- fromAndToAreFirstAndLast 
    _ <- allCollisionsIsInTheStraightPathBetweenFromAndTo
    aPathHasNoCollisions

fromAndToAreFirstAndLast :: Point -> Point -> [Collidable] -> Property
fromAndToAreFirstAndLast p1 p2 cs =
    let 
        path = getPath p1 p2 (filterCollidableToProblemSpace p1 p2 cs)
        f = head path
        l = last path
        isOutside = null [undefined | (Collidable (x, y) w h) <- cs, (Point (px, py)) <- [p1, p2], x <= px && x + w >= px && y >= py && y - h <= py]
    in
        isOutside ==> p1 == f && p2 == l

aPathHasNoCollisions :: Point -> Point -> BigList Collidable -> Property
aPathHasNoCollisions p1 p2 (BigList cs) =
    let 
        path@(p:ps) = getPath p1 p2 (filterCollidableToProblemSpace p1 p2 cs)
        isOutside = null [undefined | (Collidable (x, y) w h) <- cs, (Point (px, py)) <- [p1, p2], x <= px && x + w >= px && y >= py && y - h <= py]
    in
        isOutside ==> (null $ concat $ zipWith (getCollisions (filterCollidableToProblemSpace p1 p2 cs)) path ps)
        |-> (concat $ zipWith (getCollisions (filterCollidableToProblemSpace p1 p2 cs)) path ps)
        <?> (path, cs)

allCollisionsIsInTheStraightPathBetweenFromAndTo :: Point -> Point -> [Collidable] -> Property
allCollisionsIsInTheStraightPathBetweenFromAndTo p1 p2 cs = 
    let 
        path@(p:ps) = getPath p1 p2 (filterCollidableToProblemSpace p1 p2 cs)
        isOutside = null [undefined | (Collidable (x, y) w h) <- cs, (Point (px, py)) <- [p1, p2], x <= px && x + w >= px && y >= py && y - h <= py]
        isInTheStraightPath point = abs (pointsDistance p1 point + pointsDistance p2 point - pointsDistance p1 p2) < 0.1
    in
        isOutside ==> all isInTheStraightPath (map getPointOfCollision $ getCollisions (filterCollidableToProblemSpace p1 p2 cs) p1 p2) <<-> pointsDistance p1 p2

(<?>) :: (Testable p) => p -> ([Point], [Collidable]) -> Property
(<?>) = flip (Test.QuickCheck.whenFail . SVG.renderSVG "test.svg" (D.dims 300) . drawCollidable)
infixl 2 <?>

drawCollidable :: ([Point], [Collidable]) -> D.QDiagram SVG.B D.V2 Double D.Any
drawCollidable (path@(Point (px, py):_), cs) = mconcat [D.translate (x D.^& y) (D.rect w h) | (Collidable (x, y) w h) <- cs] D.<> (D.translate (px D.^& py) (D.strokeTrail (D.fromVertices (map toDiagramsPoint path))))
drawCollidable _ = error "hmm"
 
toDiagramsPoint (Point (x, y)) = D.p2 (x, y)
