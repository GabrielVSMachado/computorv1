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
validateLine "" = errorWithoutStackTrace "Input must be a valid Polynomial"
validateLine xs
  | '=' `notElem` xs = errorWithoutStackTrace "Equal signal must be present and the other side must be 0 or a valid Polynomial"
  | otherwise = all isAllowedChar xs
