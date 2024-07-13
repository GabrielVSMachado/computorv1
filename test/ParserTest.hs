module ParserTest (tests) where

import Distribution.TestSuite
import Parser (validateLine)
import qualified Polynomials.Types as PT
import qualified Tests as T

testEmptyString
  | not (validateLine "") = Pass
  | otherwise = Fail "Empty string isn't a valid polynomial equation"

testCompleteString
  | validateLine "5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0" = Pass
  | otherwise = Fail "5 * X^0 + 4 * X^1 - 9.3 * X^2 = 1 * X^0 has a invalid char"

testCompletePolynomial
  | PT.polynomialParse "5 * X^0 + 4 * X^1 - 9.3 * X^2" == [(0, 5.0), (1, 4.0), (2, -9.3)] = Pass
  | otherwise = Fail "5 * X^0 + 4 * X^1 - 9.3 * X^2 isnt a valid polynomial"

testNoDegreeStringWithoutEq
  | validateLine "6 * X^0 = 0" = Pass
  | otherwise = Fail "6 * X^0 has a invalid char"

tests :: IO [Test]
tests = do
  return
    [ T.test "testEmptyString" testEmptyString
    , T.test "testCompleteString" testCompleteString
    , T.test "testNoDegreeStringWithoutEq" testNoDegreeStringWithoutEq
    , T.test "testCompletePolynomial" testCompletePolynomial
    ]
