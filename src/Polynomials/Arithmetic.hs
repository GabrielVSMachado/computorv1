module Polynomials.Arithmetic (add, addSymetric) where

import Data.List (transpose)
import Polynomials.Types (Polynomial)
import Utils (quickSort)

nullPolynomial :: Polynomial
nullPolynomial = zip [0, 1 ..] [0, 0 ..]

fillPolynomial :: Int -> Polynomial -> Polynomial
fillPolynomial n p = leftPart ++ p
 where
  pDegrees = [x | (x, _) <- p]
  leftPart = [(x1, x2) | (x1, x2) <- take n nullPolynomial, x1 `notElem` pDegrees]

add :: Polynomial -> Polynomial -> Polynomial
add px py = zip [0, 1 .. higherDegree - 1] (map sum $ transpose [sx, sy])
 where
  higherDegree = maximum [x1 | (x1, _) <- px ++ py] + 1 -- first degree is zero
  sx = [x | (_, x) <- quickSort (fillPolynomial higherDegree px)]
  sy = [y | (_, y) <- quickSort (fillPolynomial higherDegree py)]

addSymetric :: Polynomial -> Polynomial -> Polynomial
addSymetric px py = add px spy
  where
    spy = map (\(y1, y2) -> (y1, -y2)) py
