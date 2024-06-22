module Polynomials.Types (Polynomial, polynomialParse) where

import Text.Read (readMaybe)
import Utils (split, trimStrings)

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
parcels ("+" : x : y : z : tokens) = unwords [x, y, z] : parcels tokens
parcels (w : x : y : "-" : tokens) = unwords [w, x, y] : parcels ("-" : tokens)
parcels (w : x : y : "+" : tokens) = unwords [w, x, y] : parcels tokens
parcels (w : x : y : z : tokens) = unwords [w, x, y, z] : parcels tokens
parcels (w : x : y : tokens) = unwords [w, x, y] : parcels tokens
parcels _ = errorWithoutStackTrace "Invalid Polynomial"

-- TODO: Make a function which takes a string or a list of strings and output a valid input to parcels function

polynomialParse :: [String] -> Polynomial
polynomialParse = map (fromParcel . \x -> read x :: Parcel) . parcels
