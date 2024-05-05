import Parser (validateLine)
import qualified Test.HUnit as UT

testEmptyString = UT.TestCase (UT.assertEqual "wrong EmptyString" (validateLine "") False)

tests = UT.TestList [UT.TestLabel "EmptyString" testEmptyString]

main = do
  UT.runTestTT tests
