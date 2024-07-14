module Polynomials.Arithmetic (add, addSymetric, roots) where

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

linearRoot :: Float -> Float -> Float
linearRoot a b = (-b) / a

quadraticRoots :: Float -> Float -> Float -> [Float]
quadraticRoots a b c
  | delta < 0 = []
  | delta == 0 = [root (sqrt delta)]
  | otherwise = quickSort [root (-sqrt delta), root (sqrt delta)]
 where
  root dlt = zeroWithoutNegative $ (-b + dlt) / (2 * a)
  zeroWithoutNegative r = case r of -0.0 -> 0.0; _ -> r
  delta = b ** 2 - 4 * a * c

roots :: Polynomial -> String
roots [(_, _)] = "Any Real number is a solution of this polynomial"
roots [(0, b), (1, a)] = "The solution is: " ++ show (linearRoot a b)
roots [(0, c), (1, b), (2, a)]
  | null secondPRoots = "There isn't any solution in real numbers."
  | length secondPRoots == 1 = "There is only one solution: " ++ show secondPRoots
  | otherwise = "There are two solutions: " ++ show secondPRoots
 where
  secondPRoots = quadraticRoots a b c
roots _ = "The polynomial degree is strictly greater than 2, I can't solve."
