import Data.Map.Strict (Map)
import Numeric.LinearAlgebra
import LeastSquares

type VarName = String
data LinExpr = Var VarName Int
             | ...
infix 4 :==:
data LinContr = LinExpr :==: LinExpr
data QuadExpr = SumSquares LinExpr
              | ProdQuad Double QuadExpr
              | SumQuad QuadExpr QuadExpr

minimize :: QuadExpr -> [LinContr]
                -> Map VarName (Vector R)
