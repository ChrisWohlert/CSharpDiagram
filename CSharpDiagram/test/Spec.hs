import CSParserTest
import TwoDVectorTest
import PathNavigatorTest
import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = do
    --hspec testCSParser 
    --hspec testTwoDVector
    hspec testPathNavigator
    --quickCheck testTwoDVectorProperties
    --quickCheck testPathNavigatorProperties