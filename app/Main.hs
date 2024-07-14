module Main where

import Parser (validateLine)
import Polynomials.Arithmetic (addSymetric, roots)
import Polynomials.Types (polynomialParse)
import Utils (split, quickSort)

main :: IO ()
main = do
  putStrLn "Insert the polynomial equation: "
  line <- getLine
  if validateLine line
    then
      let polynomials = map polynomialParse (split '=' line)
       in let results = roots $ quickSort (addSymetric (head polynomials) (last polynomials))
           in putStrLn results
    else
      errorWithoutStackTrace "Invalid char in polynomial"
