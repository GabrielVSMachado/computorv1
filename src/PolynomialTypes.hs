module PolynomialTypes (Parcel (Parcel), polynomialParse) where

import Text.Read (readMaybe)
import Utils (split, trimStrings)

type Degree = Maybe Int

type Constant = Maybe Float

data Parcel = Parcel Degree Constant deriving (Show)

type Polynomial = [(Int, Float)]

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

instance Read Parcel where
  readsPrec _ input =
    let (first, second) = sortParcelString input
        constant = readMaybe first :: Constant
        degree = degreeParser second
     in [(Parcel degree constant, "")]

fromParcel :: Parcel -> (Int, Float)
fromParcel (Parcel (Just x) (Just y)) = (x, y)
fromParcel (Parcel Nothing (Just x)) = (0, x)
fromParcel _ = error "Invalid polynomial"


parcelString :: String -> String
parcelString [] = []
parcelString _all@(x : xs)
  | x == '-' = x : parcel xs
  | x == '+' = ' ' : parcel xs
  | otherwise = parcel _all
  where
    parcel = takeWhile (`notElem` "+-")

parcels :: String -> [String]
parcels [] = []
parcels xs = parcel : parcels rest
  where
    parcel = parcelString xs
    rest = drop (length parcel) xs

polynomialParse :: String -> Polynomial
polynomialParse = map (fromParcel . \x -> read x :: Parcel) . parcels
