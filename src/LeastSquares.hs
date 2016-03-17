module LeastSquares
    ( someFunc
    , VarName
    , LinExpr(..)
    , LinContr(..)
    , QuadExpr(..)
    , minimize
    ) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Numeric.LinearAlgebra

type VarName = String

infix 4 :==:

data LinExpr = Var VarName Int
             | Coeff R
             | Vec (Vector R)
             | Mat (Matrix R)
             | Neg LinExpr
             | Sum LinExpr LinExpr
             | Prod LinExpr LinExpr
             deriving (Show, Eq)

instance Num LinExpr where
  (+) = Sum
  a - b = Sum a (Neg b)
  (*) = Prod
  negate = Neg
  signum = undefined
  abs    = undefined
  fromInteger a = Coeff (fromInteger a)

data LinContr = LinExpr :==: LinExpr

data QuadExpr = SumSquares LinExpr
              | CoeffQuad R
              | ProdQuad QuadExpr QuadExpr
              | SumQuad QuadExpr QuadExpr

instance Num QuadExpr where
  (+) = SumQuad
  (*) = ProdQuad
  (-) = undefined
  negate = undefined
  signum = undefined
  abs    = undefined
  fromInteger a = CoeffQuad (fromInteger a)

class CanonExpr e where
  varSet :: e -> S.Set (VarName, Int)
  canonicalize :: e -> M.Map VarName (Matrix R)

-- Canonicalize a generic expression
-- The vector is stored as a column matrix under the variable "".
instance CanonExpr LinExpr where
  varSet (Var v s) = S.singleton (v, s)
  varSet (Coeff _) = S.empty
  varSet (Vec _) = S.empty
  varSet (Mat _) = S.empty
  varSet (Neg e) = varSet e
  varSet (Prod _ e) = varSet e
  varSet (Sum e1 e2) = S.union (varSet e1) (varSet e2)

  canonicalize (Var v s) = M.singleton v (ident s)
  canonicalize (Vec v) = M.singleton "" (asColumn v)
  canonicalize (Neg e) = M.map (\x -> -x) $ canonicalize e
  canonicalize (Prod (Coeff c) e) = M.map (scale c) $ canonicalize e
  canonicalize (Prod (Mat m) e) = M.map (\x -> m <> x) $ canonicalize e
  canonicalize (Sum e1 e2) = M.unionWith (+) (canonicalize e1) (canonicalize e2)
  canonicalize _ = error "Expression is not well-formed"

-- Canonicalize an expression in the constraint
instance CanonExpr LinContr where
  varSet (e1 :==: e2) = S.union (varSet e1) (varSet e2)
  canonicalize (e1 :==: e2) = canonicalize (e1 - e2)

-- Canonicalize an expression in the objective, which consists of SumSquares.
instance CanonExpr QuadExpr where
  varSet (SumSquares e) = varSet e
  varSet (CoeffQuad _) = S.empty
  varSet (ProdQuad _ e) = varSet e
  varSet (SumQuad e1 e2) = S.union (varSet e1) (varSet e2)

  canonicalize (SumSquares e) = canonicalize e
  canonicalize (ProdQuad (CoeffQuad c) e) = M.map (scale (sqrt c)) $ canonicalize e
  canonicalize (SumQuad e1 e2) = M.unionWith (===) m1 m2
    where m1 = canonicalize e1
          m2 = canonicalize e2
  canonicalize _ = error "Expression is not well-formed"

minimize :: LinExpr -> [LinExpr] -> M.Map VarName (Vector R)
minimize obj [] = unpack vars $ a <\> (-b)
  where (a, b) = pack (varSet obj) canonicalForm
        vars = varSet obj
        canonicalForm = canonicalize obj
-- minimize obj constraints = unpack $ subVector 0 lengthVar result
--   where result = mat <\> vec
--         mat = fromBlocks [[2 * tr a <> a, tr c], [c, 0]]
--         vec = vjoin [2 * tr a #> b, d]
--         (a, b) = pack $ canonicalizeObj obj
--         b = undefined
--         c = undefined
--         d = undefined
--         lengthVar = snd $ size a

pack :: S.Set (VarName, Int) -> M.Map VarName (Matrix R) -> (Matrix R, Vector R)
pack vars m = (a, b)
  where a = fromBlocks [matrices]
        matrices = map getCoefficient (S.toAscList vars)
        height = fst $ size a
        b = flatten $ M.findWithDefault (konst 0 (height, 1)) "" m
        getCoefficient (varName, s) = M.findWithDefault (konst 0 (s, s)) varName m

unpack :: S.Set (VarName, Int) -> Vector R -> M.Map VarName (Vector R)
unpack vars result = M.fromList $ zip varNames coefficients
  where coefficients = map getCoefficient varList
        getCoefficient (varName, s) = subVector (startIndex varName) s result
        startIndex varName = cumsum !! fromJust (elemIndex varName varNames)
        cumsum = scanl (+) 0 varSizes
        (varNames, varSizes) = unzip varList
        varList = S.toAscList vars

someFunc :: IO ()
someFunc = putStrLn "someFunc"
