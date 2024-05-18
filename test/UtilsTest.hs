module UtilsTest (tests) where

import Distribution.TestSuite
import qualified Tests as T
import qualified Utils as Ut

testRmSpaces
  | Ut.trimStrings (Ut.splitByEq "2 * X^0 + 3 * X^1 = 4 * X^0")
      == ["2 * X^0 + 3 * X^1", "4 * X^0"] =
      Pass
  | otherwise = Fail (show $ Ut.trimStrings (Ut.splitByEq "2 * X^0 + 3 * X^1 = 4 * X^0"))

testSplitPolinomial
  | Ut.splitByEq "2 * X^0 + 3 * X^1 = 4 * X^0" == ["2 * X^0 + 3 * X^1 ", " 4 * X^0"] = Pass
  | otherwise = Fail (show $ Ut.splitByEq "2 * X^0 + 3 * X^1 = 4 * X^0")

tests :: IO [Test]
tests = do
  return
    [ T.test "testSplitPolinomial" testSplitPolinomial,
      T.test "testRmSpaces" testRmSpaces
    ]
