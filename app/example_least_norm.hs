import Data.Map.Strict ((!))
import Numeric.LinearAlgebra hiding ((!))
import LeastSquares

leastSquaresEstimate :: Vector R
leastSquaresEstimate = result ! "x"
  where result = minimize (SumSquares x) [c * x :==: d]
        c = Mat $ (2><3) [3.0, 7.0, -1.5, 2.8, 10.0, -5.3]
        d = Vec $ vector [-1.0, 3.5]
        x = Var "x" 3
