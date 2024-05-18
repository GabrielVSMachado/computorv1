module Parser (validateLine) where

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
