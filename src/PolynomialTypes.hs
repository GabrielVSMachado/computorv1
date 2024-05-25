module PolynomialTypes (Parcel (Parcel)) where

import Text.Read (readMaybe)
import Utils (split, trimStrings)

type Degree = Maybe Int

type Constant = Maybe Float

data Parcel = Parcel Degree Constant deriving (Show)

degreeParser :: String -> Degree
degreeParser s
  | s `elem` commonDegreePattern = Just (read (drop 2 s) :: Int)
  | otherwise = Nothing
  where
    commonDegreePattern = ["X^" ++ [y] | y <- ['0' .. '9']]

sortParcelString :: String -> (String, String)
sortParcelString input
  | 'X' `elem` head values = (last values, head values)
  | otherwise = (head values, last values)
  where
    values = trimStrings $ split '*' input

-- TODO: Parse a full polinomyal like '5 * X^0 + 4 * X^1 - 9.3 * X^2';

-- TODO: Parse the received polynomial to resolved to a allowed format
-- ex.: 1/2 * X^1 + 5 * X^0 turn into 0.5 * X^1 + 5 * X^0

instance Read Parcel where
  readsPrec _ input =
    let (first, second) = sortParcelString input
        constant = readMaybe first :: Constant
        degree = degreeParser second
     in [(Parcel degree constant, "")]
