module LeastSquares
    ( VarName
    , LinExpr(..)
    , LinContr(..)
    , QuadExpr(..)
    , minimize
    ) where

import Data.List (elemIndex, foldl1')
import qualified Data.Map.Strict as M
import Data.Maybe (fromJust)
import qualified Data.Set as S
import Numeric.LinearAlgebra

type VarName = String

type CanonicalMap = M.Map VarName (Matrix R)

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
    abs = undefined
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
    abs = undefined
    fromInteger a = CoeffQuad (fromInteger a)

class CanonExpr e  where
    varSet :: e -> S.Set (VarName, Int)
    canonicalize :: e -> CanonicalMap

-- Canonicalize a generic expression
-- The vector is stored as a column matrix under the variable "".
instance CanonExpr LinExpr where
    varSet (Var v s) = S.singleton (v, s)
    varSet (Coeff _) = S.empty
    varSet (Vec _) = S.empty
    varSet (Mat _) = S.empty
    varSet (Neg e) = varSet e
    varSet (Prod _ e) = varSet e
    varSet (Sum e1 e2) = varSet e1 `S.union` varSet e2
    canonicalize (Var v s) = M.singleton v (ident s)
    canonicalize (Vec v) = M.singleton "" (asColumn v)
    canonicalize (Neg e) = M.map negate $ canonicalize e
    canonicalize (Prod (Coeff c) e) = M.map (scale c) $ canonicalize e
    canonicalize (Prod (Mat m) e) = M.map (m <>) $ canonicalize e
    canonicalize (Sum e1 e2) =
        M.unionWith (+) (canonicalize e1) (canonicalize e2)
    canonicalize _ = error "Expression is not well-formed"

-- Canonicalize an expression in the constraint
instance CanonExpr LinContr where
    varSet (e1 :==: e2) = varSet e1 `S.union` varSet e2
    canonicalize (e1 :==: e2) = canonicalize (e1 - e2)

-- Canonicalize an expression in the objective, which consists of SumSquares.
instance CanonExpr QuadExpr where
    varSet (SumSquares e) = varSet e
    varSet (CoeffQuad _) = S.empty
    varSet (ProdQuad _ e) = varSet e
    varSet (SumQuad e1 e2) = varSet e1 `S.union` varSet e2
    canonicalize (SumSquares e) = canonicalize e
    canonicalize (ProdQuad (CoeffQuad c) e) =
        M.map (scale (sqrt c)) $ canonicalize e
    canonicalize (SumQuad e1 e2) = canonicalize e1 `vstack` canonicalize e2
    canonicalize _ = error "Expression is not well-formed"

-- Combine two canonical maps by stacking one on top of another.
vstack :: CanonicalMap -> CanonicalMap -> CanonicalMap
vstack m1 m2 = M.fromSet combine (M.keysSet m1 `S.union` M.keysSet m2)
  where
    combine k = lookupDefault k m1 === lookupDefault k m2
    lookupDefault k m = M.findWithDefault (konst 0 (firstDim m, 1)) k m
    firstDim = rows . snd . fromJust . M.lookupGE "" :: CanonicalMap -> Int

minimize :: QuadExpr -> [LinContr] -> M.Map VarName (Vector R)
minimize obj [] = unpack vars $ a <\> (-b)
  where
    (a,b) = pack vars . canonicalize $ obj
    vars = varSet obj
minimize obj constraints = unpack vars result
  where
    result = mat <\> (-vec)
    mat = fromBlocks [[2 * tr a <> a, tr c], [c, 0]]
    vec = vjoin [2 * tr a #> b, d]
    (a,b) = pack vars . canonicalize $ obj
    (c,d) = pack vars $ foldl1' vstack cds
    cds = map canonicalize constraints
    vars = varSet obj `S.union` S.unions (map varSet constraints)

pack :: S.Set (VarName, Int) -> CanonicalMap -> (Matrix R, Vector R)
pack vars m = (a, b)
  where
    a = fromBlocks [map getCoefficient (S.toAscList vars)]
    b = flatten $ M.findWithDefault (konst 0 (rows a, 1)) "" m
    getCoefficient (varName,s) = M.findWithDefault (konst 0 (s, s)) varName m

-- The vector given to unpack might be longer than the total size of the
-- variables. It might contain the dual variable values, for example.
unpack :: S.Set (VarName, Int) -> Vector R -> M.Map VarName (Vector R)
unpack vars result = M.fromList $ zip varNames coefficients
  where
    coefficients = map getCoefficient varList
    getCoefficient (varName,s) = subVector (startIndex varName) s result
    startIndex varName = cumsum !! fromJust (elemIndex varName varNames)
    cumsum = scanl (+) 0 varSizes
    (varNames,varSizes) = unzip varList
    varList = S.toAscList vars
