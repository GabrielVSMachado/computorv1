module Parser (validateLine) where

isAllowedChar :: Char -> Bool
isAllowedChar x
  | x `elem` validChars = True
  | x `elem` numbersAsChar = True
  | otherwise = False
  where
    validChars = ['X', '+', '*', '-', '^', ' ', '=', '.']
    numbersAsChar = ['0' .. '9']

validateLine :: String -> Bool
validateLine "" = False
validateLine xs
  | '=' `notElem` xs = False
  | length [x | x <- xs, x == '='] > 1 = False
  | otherwise = all isAllowedChar xs
