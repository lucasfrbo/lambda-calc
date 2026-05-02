module Eval where

import Parser
import Data.List (elemIndex)
import Data.Map (Map)
import qualified Data.Map as Map

type FuncsMap = Map String Expr

expand :: FuncsMap  -> Expr -> Expr
expand fs (Appl f x) = Appl (expand fs f) (expand fs x)
expand fs (Def var expr) = Def var (expand fs expr)
expand fs (Var name d)
    | d == (-1) = expand fs $ fs Map.! name
    | otherwise = Var name d

calcBruijn :: Expr -> Expr
calcBruijn = f []
    where
        f :: [String] -> Expr -> Expr
        f vars (Appl ex1 ex2) = Appl (f vars ex1) (f vars ex2)
        f vars (Def var expr) = Def var $ f (var : vars) expr
        f vars (Var name _) = Var name $ maybe (-1) (+1) (elemIndex name vars)

betaReduce :: Expr -> Expr
betaReduce (Appl (Def _ f) x) = bRedDepth 1 f x
    where
        bRedDepth :: Int -> Expr -> Expr -> Expr
        bRedDepth d (Def var expr) x = Def var (bRedDepth (d+1) expr x)
        bRedDepth d (Var name vd)  x = if d == vd then x else Var name vd
        bRedDepth d (Appl e1 e2)   x = Appl (bRedDepth d e1 x) (bRedDepth d e2 x)

eval' :: FuncsMap -> Expr -> Expr
eval' fs appl@(Appl      (Def  {}) _) = eval fs $ betaReduce appl
eval' fs      (Appl appl@(Appl {}) x) = eval fs $ Appl (eval fs appl) x
eval' fs      (Appl (Var name d) x) 
    | d == (-1) = eval fs $ Appl (fs Map.! name) x
    | otherwise = Appl (Var name d) x

eval' fs other = other

eval :: FuncsMap -> Expr -> Expr
eval fs expr = case expr of
    Appl (Def _ _) _  -> eval fs $ betaReduce expr
    Appl (Appl f y) x -> eval fs $ Appl (eval fs $ Appl f y) x
    Appl (Var name d) x -> if d == (-1)
        then eval fs $ Appl (fs Map.! name) x
        else Appl (Var name d) x
        
    -- Def var body -> Def var $ eval fs body
    other -> other