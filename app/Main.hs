module Main where

import Numeric.LinearAlgebra
import LeastSquares

main :: IO ()
main = do
  let a = Mat $ (3><2) [1.0, 1.0, 1.0, 4.0, 2.0, -1.0]
  let b = Vec $ vector [13, 27, 1]
  let x = Var "x" 2
  print $ minimize (SumSquares (a * x - b)) []
  let a1 = Mat $ (3><1) [1.0, 1.0, 1.0]
  let a2 = Mat $ (3><1) [2.0, 4.0, -1.0]
  let x1 = Var "x1" 1
  let x2 = Var "x2" 1
  print $ minimize (SumSquares (a * x - b) + 2 * SumSquares x) []
