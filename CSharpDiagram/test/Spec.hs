import CSParserTest
import Test.Hspec

main :: IO ()
main = hspec $ testCSParser >>= testPathNavigator
    