module TwoDVectorTest where

    
import Test.Hspec
import TwoDVector
import Data.Maybe
import Test.QuickCheck
import Arbitraries

rightUpV = Vector (1, 1)
rightDownV = Vector (1, -1)
leftUpV = Vector (-1, 1)
leftDownV = Vector (-1, -1)
upV = Vector (0, 1)
rightV = Vector (1, 0)
downV = Vector (0, -1)
leftV = Vector (-1, 0)

rightUpPV = PointVector (Point (0, 0)) rightUpV
rightDownPV = PointVector (Point (0, 0)) rightDownV
leftUpPV = PointVector (Point (0, 0)) leftUpV
leftDownPV = PointVector (Point (0, 0)) leftDownV

straightRightV = fromJust $ makeStraightVector (PointVector (Point (0, 5)) (Vector (1, 0)))
straightLeftV = fromJust $ makeStraightVector (PointVector (Point (0, 5)) (Vector (-1, 0)))
straightDownV = fromJust $ makeStraightVector (PointVector (Point (5, 0)) (Vector (0, -1)))
straightUpV = fromJust $ makeStraightVector (PointVector (Point (5, 0)) (Vector (0, 1)))


testTwoDVector = do 
    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - RightUp -> StraightRight" $
        getVectorIntersection rightUpPV straightRightV `shouldBe` (Just (Point (5, 5)))
        
    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - RightDown -> StraightRight" $
        getVectorIntersection rightDownPV straightRightV `shouldBe` (Just (Point (-5, 5)))
        
    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - LeftUp -> StraightRight" $
        getVectorIntersection leftUpPV straightRightV `shouldBe` (Just (Point (-5, 5)))
        
    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - LeftDown -> StraightRight" $
        getVectorIntersection leftDownPV straightRightV `shouldBe` (Just (Point (5, 5)))

    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - RightUp -> StraightLeft" $
        getVectorIntersection rightUpPV straightLeftV `shouldBe` (Just (Point (5, 5)))
        
    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - RightDown -> StraightLeft" $
        getVectorIntersection rightDownPV straightLeftV `shouldBe` (Just (Point (-5, 5)))
        
    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - LeftUp -> StraightLeft" $
        getVectorIntersection leftUpPV straightLeftV `shouldBe` (Just (Point (-5, 5)))
        
    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - LeftDown -> StraightLeft" $
        getVectorIntersection leftDownPV straightLeftV `shouldBe` (Just (Point (5, 5)))

    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - RightUp -> StraightLeft" $
        getVectorIntersection rightUpPV straightDownV `shouldBe` (Just (Point (5, 5)))
        
    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - RightDown -> StraightLeft" $
        getVectorIntersection rightDownPV straightDownV `shouldBe` (Just (Point (5, -5)))
        
    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - LeftUp -> StraightLeft" $
        getVectorIntersection leftUpPV straightDownV `shouldBe` (Just (Point (5, -5)))
        
    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - LeftDown -> StraightLeft" $
        getVectorIntersection leftDownPV straightDownV `shouldBe` (Just (Point (5, 5)))

    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - RightUp -> StraightLeft" $
        getVectorIntersection rightUpPV straightUpV `shouldBe` (Just (Point (5, 5)))
        
    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - RightDown -> StraightLeft" $
        getVectorIntersection rightDownPV straightUpV `shouldBe` (Just (Point (5, -5)))
        
    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - LeftUp -> StraightLeft" $
        getVectorIntersection leftUpPV straightUpV `shouldBe` (Just (Point (5, -5)))
        
    describe "TwoDVector.getVectorIntersection" $
        it "returns the point of intersection between a vector and a straight vector - LeftDown -> StraightLeft" $
        getVectorIntersection leftDownPV straightUpV `shouldBe` (Just (Point (5, 5)))
        
    describe "TwoDVector.(.-.)" $
        it "returns the vector between two points" $
        (.-.) (Point (2, 2)) (Point (5, 5)) `shouldBe` (PointVector (Point (2, 2)) (Vector (3, 3)))
        
    describe "TwoDVector.(.-.)" $
        it "returns the vector between two points" $
        (.-.) (Point (2, 2)) (Point (-5, 5)) `shouldBe` (PointVector (Point (2, 2)) (Vector (-7, 3)))
        
    describe "TwoDVector.(.-.)" $
        it "returns the vector between two points" $
        (.-.) (Point (2, 2)) (Point (-5, -5)) `shouldBe` (PointVector (Point (2, 2)) (Vector (-7, -7)))
        
    describe "TwoDVector.(.-.)" $
        it "returns the vector between two points" $
        (.-.) (Point (2, 2)) (Point (5, -5)) `shouldBe` (PointVector (Point (2, 2)) (Vector (3, -7)))
        
    describe "TwoDVector.getCardinalDirection" $
        it "returns the cardinal direction of a vector" $
        getCardinalDirection upV `shouldBe` North
        
    describe "TwoDVector.getCardinalDirection" $
        it "returns the cardinal direction of a vector" $
        getCardinalDirection rightUpV `shouldBe` NorthEast
        
    describe "TwoDVector.getCardinalDirection" $
        it "returns the cardinal direction of a vector" $
        getCardinalDirection rightV `shouldBe` East
        
    describe "TwoDVector.getCardinalDirection" $
        it "returns the cardinal direction of a vector" $
        getCardinalDirection rightDownV `shouldBe` SouthEast
        
    describe "TwoDVector.getCardinalDirection" $
        it "returns the cardinal direction of a vector" $
        getCardinalDirection downV `shouldBe` South
        
    describe "TwoDVector.getCardinalDirection" $
        it "returns the cardinal direction of a vector" $
        getCardinalDirection leftDownV `shouldBe` SouthWest
        
    describe "TwoDVector.getCardinalDirection" $
        it "returns the cardinal direction of a vector" $
        getCardinalDirection leftV `shouldBe` West
        
    describe "TwoDVector.getCardinalDirection" $
        it "returns the cardinal direction of a vector" $
        getCardinalDirection leftUpV `shouldBe` NorthWest



testTwoDVectorProperties = getVectorIntersectionIsOnCorrectAxis

getVectorIntersectionIsOnCorrectAxis :: PointVector -> StraightVector -> Property
getVectorIntersectionIsOnCorrectAxis p1@(PointVector (Point (x1, y1)) (Vector (vx1, vy1))) vv@(VerticalVector p2@(PointVector (Point (x2, y2)) (Vector (vx2, vy2)))) = 
    p1 /= p2 && vx1 /= 0 ==>
        let
            Just (Point (x, y)) = getVectorIntersection p1 vv
        in
            abs (x - x2) <= 0.0001 <->> x2 <<-> x
getVectorIntersectionIsOnCorrectAxis p1@(PointVector (Point (x1, y1)) (Vector (vx1, vy1))) hv@(HorizontalVector p2@(PointVector (Point (x2, y2)) (Vector (vx2, vy2)))) = 
    p1 /= p2 && vy1 /= 0 ==>
        let
            Just (Point (x, y)) = getVectorIntersection p1 hv
        in
            abs (y - y2) <= 0.0001 <->> y2 <<-> y
