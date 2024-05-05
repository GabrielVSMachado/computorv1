module Main where

import Control.Monad (when)
import Parser (validateLine)

main :: IO ()
main = do
  line <- getLine
  when (validateLine line) $ do
    putStrLn line

  putStrLn "Invalid polynomial equation form"
