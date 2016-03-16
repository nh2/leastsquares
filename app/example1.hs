import qualified Data.Map.Strict as M
import Numeric.LinearAlgebra
import LeastSquares

minimize :: Expr -> [Expr] -> M.Map VarName (Vector R)
