module UtilsTest (tests) where

import Distribution.TestSuite
import qualified Tests as T
import qualified Utils as Ut

splitByEq = Ut.split '='

testRmSpaces
  | Ut.trimStrings (splitByEq "2 * X^0 + 3 * X^1 = 4 * X^0")
      == ["2 * X^0 + 3 * X^1", "4 * X^0"] =
      Pass
  | otherwise = Fail (show $ Ut.trimStrings (splitByEq "2 * X^0 + 3 * X^1 = 4 * X^0"))

testSplitPolinomial
  | splitByEq "2 * X^0 + 3 * X^1 = 4 * X^0" == ["2 * X^0 + 3 * X^1 ", " 4 * X^0"] = Pass
  | otherwise = Fail (show $ splitByEq "2 * X^0 + 3 * X^1 = 4 * X^0")

tests :: IO [Test]
tests = do
  return
    [ T.test "testSplitPolinomial" testSplitPolinomial,
      T.test "testRmSpaces" testRmSpaces
    ]
