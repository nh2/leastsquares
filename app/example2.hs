import Data.Map.Strict ((!))
import Numeric.LinearAlgebra hiding ((!))
import LeastSquares

leastSquaresEstimate :: Vector R
leastSquaresEstimate = result ! "x"
  where result = minimize (SumSquares (a*x-b)) [c*x:==:d]
        a = Mat $ (3><2) [1.0, 1.0, 1.0, 4.0, 2.0, -1.0]
        b = Vec $ vector [13.0, 27.0, 1.0]
        c = Mat $ (1><2) [3.0, 7.0]
        d = Vec $ vector [-1.0]
        x = Var "x" 2
