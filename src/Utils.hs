module Utils (split, trimStrings, quickSort) where

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

trimStrings :: [String] -> [String]
trimStrings = foldr' (\x acc -> trim x : acc) []

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
  let lower = filter (<= x) xs
      greater = filter (> x) xs
   in quickSort lower ++ [x] ++ quickSort greater
