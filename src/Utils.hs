module Utils (splitByEq, trimStrings) where

import Data.Foldable (Foldable (foldr'))

trimStart :: String -> String
trimStart "" = ""
trimStart (' ' : xs) = trimStart xs
trimStart xs = xs

trimEnd :: String -> String
trimEnd = reverse . trimStart . reverse

trim :: String -> String
trim = trimEnd . trimStart

split :: Char -> String -> [String]
split _ "" = []
split c xs =
  let ys = takeWhile (/= c) xs
      zs = drop (length ys + 1) xs
   in ys : split c zs

splitByEq :: String -> [String]
splitByEq = split '='

trimStrings :: [String] -> [String]
trimStrings = foldr' (\x acc -> trim x : acc) []
