module ParserTest (tests) where

import Distribution.TestSuite
import Parser (splitByEq, validateLine)
import qualified Tests as T

testEmptyString :: Result
testEmptyString
  | not (validateLine "") = Pass
  | otherwise = Fail "Empty string isn't a valid polynomial equation"

testCompleteString :: Result
testCompleteString
  | validateLine "5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0" = Pass
  | otherwise = Fail "5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0 has a invalid char"

testNoDegreeStringWithoutEq :: Result
testNoDegreeStringWithoutEq
  | validateLine "6 * X^0" = Pass
  | otherwise = Fail "6 * X^0 has a invalid char"


tests :: IO [Test]
tests = do
  return
    [ T.test "testEmptyString" testEmptyString,
      T.test "testCompleteString" testCompleteString,
      T.test "testNoDegreeStringWithoutEq" testNoDegreeStringWithoutEq,
    ]
