module Parser (validateLine, splitByEq) where

import Utils (trimDSTuple)

isAllowedChar :: Char -> Bool
isAllowedChar x
  | x `elem` validChars = True
  | x `elem` numbersAsChar = True
  | otherwise = False
  where
    validChars = ['X', '+', '*', '-', '/', '^', ' ', '=', '.']
    numbersAsChar = ['0' .. '9']

validateLine :: String -> Bool
validateLine "" = False
validateLine xs = all isAllowedChar xs

splitByEq :: String -> (String, String)
splitByEq xs = case splitedByEq of
  (_, "") -> trimDSTuple splitedByEq
  (ys, _ : zs) -> trimDSTuple (ys, zs)
  where
    splitedByEq = span (/= '=') xs
