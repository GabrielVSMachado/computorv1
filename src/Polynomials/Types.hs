module Polynomials.Types (Polynomial, polynomialParse, isAllowedDegree) where

import Data.List (intersperse)
import Text.Read (readMaybe)
import Utils (quickSort, split, trimStrings)

type Degree = Maybe Int

type Constant = Maybe Float

data Parcel = Parcel Degree Constant deriving (Show)

type Polynomial = [(Int, Float)]

sortParcelString :: String -> (String, String)
sortParcelString input
  | 'X' `elem` head values = (last values, head values)
  | otherwise = (head values, last values)
 where
  values = trimStrings $ split '*' input

instance Read Parcel where
  readsPrec _ input =
    let (first, second) = sortParcelString input
        constant = readMaybe first :: Constant
        degree = readMaybe (drop 2 second) :: Degree
     in [(Parcel degree constant, "")]

fromParcel :: Parcel -> (Int, Float)
fromParcel (Parcel (Just x) (Just y)) = (x, y)
fromParcel (Parcel Nothing (Just x)) = (0, x)
fromParcel _ = errorWithoutStackTrace "Invalid polynomial"

parcels :: [String] -> [String]
parcels [] = []
parcels ("-" : v : x : "X" : "^" : z : tokens) = unwords ["-", v, x, "X^", z] : parcels tokens
parcels ("X" : "^" : x : w : "-" : z : tokens) = unwords ["X^", x, w, "-", z] : parcels tokens
parcels ("+" : "X" : "^" : x : "*" : z : tokens) = unwords ["X^", x, "*", z] : parcels tokens
parcels ("+" : v : "*" : "X" : "^" : z : tokens) = unwords [v, "*", "X^", z] : parcels tokens
parcels (v : "*" : "X" : "^" : z : tokens) = unwords [v, "*", "X^", z] : parcels tokens
parcels xs = errorWithoutStackTrace ("Invalid Polynomial, the wrong part is: " ++ unwords xs)

-- TODO: code the function intersperse in module Utils

polynomialParse :: String -> Polynomial
polynomialParse = map (fromParcel . \x -> read x :: Parcel) . parcels . words . intersperse ' '

isAllowedDegree :: Polynomial -> Bool
isAllowedDegree px
  | degree > 2 = errorWithoutStackTrace (show degree ++ " isn't supported by this project")
  | otherwise = True
 where
  whichDegree = fst . last . quickSort
  degree = whichDegree px
