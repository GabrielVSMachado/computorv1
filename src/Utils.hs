module Utils (split, trimStrings, quickSort, intersperseBySpaceExcptNumber) where

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

intersperseBySpaceExcptNumber :: String -> String
intersperseBySpaceExcptNumber [] = []
intersperseBySpaceExcptNumber [x] = [x]
intersperseBySpaceExcptNumber (x : xs)
  | x `elem` ['0' .. '9'] =  (x: fst numbers) ++ ' ' : intersperseBySpaceExcptNumber (snd numbers)
  | otherwise = x : ' ' : intersperseBySpaceExcptNumber xs
  where
    numbers = span (\z -> z == '.' || z `elem` ['0'..'9']) xs
