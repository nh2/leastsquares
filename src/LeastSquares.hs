module LeastSquares
    ( someFunc
    , Expr
    , canonicalizeObj
    ) where

-- import Numeric.LinearAlgebra (Matrix)
import Numeric.LinearAlgebra
import qualified Data.Map.Strict as M

type VarName = String

infix 4 :==:

-- data Expr a = Var { varName :: VarName
--                   , varDimension :: Int}
data Expr = Var VarName Int
          | Coeff R
          | Vec (Vector R)
          | Mat (Matrix R)
          | Neg Expr
          | Sum Expr Expr
          | Prod Expr Expr
          | SumSquares Expr
          | Expr :==: Expr
          deriving (Show, Eq)

instance Num Expr where
  (+) = Sum
  a - b = Sum a (Neg b)
  (*) = Prod
  negate = Neg
  signum = undefined
  abs    = undefined
  fromInteger a = Coeff (fromInteger a)

-- Canonicalize a generic expression
canonicalize :: Expr -> M.Map VarName (Matrix R)
canonicalize (Var v s) = M.singleton v (ident s)
canonicalize (Vec v) = M.singleton "" (asColumn v)
canonicalize (Neg e) = M.map (\x -> -x) $ canonicalize e
canonicalize (Prod (Coeff c) e) = M.map (scale c) $ canonicalize e
canonicalize (Prod (Mat m) e) = M.map (\x -> m <> x) $ canonicalize e
canonicalize (Sum e1 e2) = M.unionWith (+) (canonicalize e1) (canonicalize e2)
canonicalize _ = error "Expression is not well-formed"

-- Canonicalize an expression in the objective, which consists of SumSquares.
-- The vector is stored as a column matrix under the variable "".
canonicalizeObj :: Expr -> M.Map VarName (Matrix R)
canonicalizeObj (SumSquares e) = canonicalizeObj e
canonicalizeObj (Sum (SumSquares e1) (SumSquares e2)) = M.unionWith (===) m1 m2
  where m1 = canonicalizeObj e1
        m2 = canonicalizeObj e2
canonicalizeObj (Sum (SumSquares e1) (Prod (Coeff c) (SumSquares e2))) = M.unionWith (===) m1 m2
  where m1 = canonicalizeObj e1
        m2 = M.map (scale (sqrt c)) $ canonicalizeObj e2
canonicalizeObj _ = error "Expression is not well-formed"

-- Canonicalize an expression in the constraint, which consists of SumSquares
-- canonicalizeConstraint :: Expr -> (Matrix R, Vector R)
-- canonicalizeConstraint (Sum e1 e2) = (m1 + m2, v1 + v2)
--   where (m1, v1) = canonicalizeConstraint e1
--         (m2, v2) = canonicalizeConstraint e2
-- canonicalizeConstraint (Prod (Coeff c) e) = (scale c m, scale c v)
--   where (m, v) = canonicalizeConstraint e
-- canonicalizeConstraint (e1 :==: e2) = canonicalizeConstraint (e1 - e2)
-- canonicalizeConstraint e = canonicalize e

someFunc :: IO ()
someFunc = putStrLn "someFunc"
