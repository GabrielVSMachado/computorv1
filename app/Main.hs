module Main where

import Parser (validateLine)
import Polynomials.Types (polynomialParse)
import Utils ( split )

main :: IO ()
main = do
  line <- getLine
  if not $ validateLine line
    then
      putStrLn "Invalid polynomial equation form"
    else
        let polynomials = (Utils.split '=' line) in
        print (polynomialParse (last polynomials))
        -- print (add (head polynomials) (last polynomials), polynomials)
