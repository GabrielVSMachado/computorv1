module UtilsTest (tests) where

import Distribution.TestSuite
import qualified Tests as T
import qualified Utils as Ut

testEmptyString
  | Ut.trim "" == "" = Pass
  | otherwise = Fail "Error on empty strings"

testComplete
  | Ut.trim "    Hello World     " == "Hello World" = Pass
  | otherwise = Fail "testComplete"

testStart
  | Ut.trim "    H" == "H" = Pass
  | otherwise = Fail "testStart"

testEnd
  | Ut.trim "H     " == "H" = Pass
  | otherwise = Fail "testEnd"

tests :: IO [Test]
tests = do
  return
    [ T.test "testEmptyString" testEmptyString,
      T.test "testStart" testStart,
      T.test "testEnd" testEnd,
      T.test "testComplete" testComplete
    ]
