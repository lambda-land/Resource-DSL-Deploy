module DSL.Check where

import Data.Maybe (fromJust)
import Data.SBV

import DSL.Type


-- | Convert a predicate to a symbolic integer.
symPredI :: [(Var,SInteger)] -> Pred -> SInteger
symPredI _  (PI i)     = fromInteger (toInteger i)
symPredI is (PRef v)   = fromJust (lookup v is)
symPredI is (PAdd l r) = symPredI is l + symPredI is r

-- | Convert a predicate to a symbolic boolean.
symPredB :: [(Var,SBool)] -> [(Var,SInteger)] -> Pred -> SBool
symPredB bs _  (PB b)     = fromBool b
symPredB bs _  (PRef v)   = fromJust (lookup v bs)
symPredB bs is (PNot e)   = bnot (symPredB bs is e)
symPredB bs is (PAnd l r) = symPredB bs is l &&& symPredB bs is r
symPredB bs is (POr  l r) = symPredB bs is l ||| symPredB bs is r
symPredB _  is (PLte l r) = symPredI is l .<= symPredI is r
symPredB _  is (PEqu l r) = symPredI is l .== symPredI is r

-- | Convert a predicate to a symbolic boolean.
symPred :: [Var] -> [Var] -> Pred -> Symbolic SBool
symPred bvs ivs p = do
    bs <- mapM exists bvs
    is <- mapM exists ivs
    return (symPredB (zip bvs bs) (zip ivs is) p)
