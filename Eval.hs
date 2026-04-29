module Eval where

import Parser
import Data.List (elemIndex)

-- TODO: Beta substitution
-- TODO: Keep global symbols

calcBruijn :: Expr -> Expr
calcBruijn = f []
    where
        f :: [String] -> Expr -> Expr
        f vars (Appl ex1 ex2) = Appl (f vars ex1) (f vars ex2)
        f vars (Def var@(Var x _) expr) = Def var $ f (x : vars) expr
        f vars (Var name _) = Var name $ maybe (-1) (+1) (elemIndex name vars)

-- betaReduce is a single reduction on a
betaReduce :: Expr -> Expr
-- 0 or 1???
betaReduce (Appl (Def _ f) x) = bRedDepth 1 f x
    where
        bRedDepth :: Int -> Expr -> Expr -> Expr
        bRedDepth d (Def var expr) x = Def var (bRedDepth (d+1) expr x)
        bRedDepth d var@(Var _ vd) x = if d == vd then x else var
        bRedDepth d (Appl e1 e2)   x = Appl (bRedDepth d e1 x) (bRedDepth d e2 x)

eval :: Expr -> Expr
eval var@(Var {}) = var
eval (Def var expr) = Def var $ eval expr

eval appl@(Appl      (Def  {}) _) = eval $ betaReduce appl
eval      (Appl appl@(Appl {}) x) = eval $ Appl (eval appl) x
eval      (Appl var@(Var name 0) x) = undefined
eval      (Appl var@(Var {}) x) = Appl var x