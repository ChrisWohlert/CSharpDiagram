module Arbitraries where

import Test.QuickCheck
import TwoDVector
import PathNavigator
import Data.Maybe

instance Arbitrary Point where
    arbitrary = do
        x <- choose (-1000, 1000)
        y <- choose (-1000, 1000)
        return $ Point (x, y)
    shrink (Point (x, y)) = [Point (sx, sy) | sx <- shrink x, sy <- shrink y]

instance Arbitrary Vector where
    arbitrary = do
        x <- arbitrary
        y <- arbitrary
        return $ Vector (x, y)
    shrink (Vector (x, y)) = [Vector (sx, sy) | sx <- shrink x, sy <- shrink y]

instance Arbitrary PointVector where
    arbitrary =  PointVector <$> arbitrary <*> arbitrary
    shrink (PointVector point vector) = [PointVector sPoint sVector | sPoint <- shrink point, sVector <- shrink vector]

instance Arbitrary StraightVector where
    arbitrary = do
        n <- choose (0 :: Int, 1)
        point <- arbitrary
        case n > 0 of
            True -> return $ fromJust $ makeStraightVector $ PointVector point (Vector (0, 1))
            False -> return $ fromJust $ makeStraightVector $ PointVector point (Vector (1, 0))

instance Arbitrary Collidable where
    arbitrary = do
        x <- choose (-1000, 1000)
        y <- choose (-1000, 1000)
        h <- choose (1, 500)
        return $ Collidable (x, y) 40 h

newtype TinyList a = TinyList [a] deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (TinyList a) where
    arbitrary = sized $ \ s -> do
        n <- choose (0, s `min` 5)
        xs <- vectorOf n arbitrary
        return $ TinyList xs

newtype BigList a = BigList [a] deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (BigList a) where
    arbitrary = sized $ \ s -> do
        n <- choose (0, s `max` 30)
        xs <- vectorOf n arbitrary
        return $ BigList xs




(<->>) :: (Testable p, Show s) => p -> s -> Property
(<->>) = flip (Test.QuickCheck.counterexample . show . ("Expected: " ++) . show)
infixl 2 <->>

(<<->) :: (Testable p, Show s) => p -> s -> Property
(<<->) = flip (Test.QuickCheck.counterexample . show . ("Got: " ++) . show)
infixl 2 <<->

(|->) :: (Testable p, Show s) => p -> s -> Property
(|->) = flip (Test.QuickCheck.counterexample . show)
infixl 2 |->