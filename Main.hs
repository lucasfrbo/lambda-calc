module Main where

import Parser
import Eval
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

processInput :: FuncsMap -> String -> (FuncsMap, String)
processInput fs input = maybe (fs, "ERROR: Wrong input") f (run stmntP input)
    where
        f :: (String, Statement) -> (FuncsMap, String)
        f ("", Eval expr) = (fs, (show . eval fs . calcBruijn) expr)
        f ("", def@(Define (Var name _) expr)) = (Map.insert name (calcBruijn expr) fs, show def)

processInput' :: FuncsMap -> String -> (FuncsMap, String)
processInput' fs input = case run stmntP input of
    Just ("", result) -> case result of
        Eval expr                      -> (fs, show $ eval fs $ calcBruijn expr)
        def@(Define (Var name _) expr) -> (Map.insert name (calcBruijn expr) fs, show def)
-- name ++ " := " ++ show expr
    Just _  -> (fs, "ERROR: wrong input")
    Nothing -> (fs, "ERROR: wrong input")

replLoop :: FuncsMap -> IO ()
replLoop fs = do
    putStr ">> " --TODO (change)
    input <- getLine
    let (fs', output) = processInput' fs input
    putStrLn output
    -- print fs' DEBUG
    replLoop fs'
        

main :: IO ()
main = do
    putStrLn "Lambda Calc REPL"
    replLoop Map.empty