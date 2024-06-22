module Polynomials.Arithmetic (add) where

import Data.List (transpose)
import Polynomials.Types (Polynomial)
import Utils (quickSort)

add :: Polynomial -> Polynomial -> Polynomial
add px py = zip [0, 1 .. degree] (map sum $ transpose [cx, cy])
  where
    lengthPx = length px
    lengthPy =  length py
    higher = max lengthPx lengthPy
    degree = higher - 1
    cx = replicate (higher - lengthPx) 0 ++ [x | (_, x) <- quickSort px]
    cy = replicate (higher - lengthPy) 0 ++ [y | (_, y) <- quickSort py]
