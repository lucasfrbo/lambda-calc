module Main where

import Parser
import Eval
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map


processInput :: FuncsMap -> String -> (FuncsMap, String)
processInput fs input = case runParser stmntP input of
    Just ("", result) -> case result of
        Eval        expr -> (fs, show $ eval fs $ calcBruijn expr)
        Define name expr -> (Map.insert name (calcBruijn expr) fs, name ++ " := " ++ show expr)
    Just _  -> (fs, "ERROR: wrong input")
    Nothing -> (fs, "ERROR: wrong input")

replLoop :: FuncsMap -> IO ()
replLoop fs = do
    putStr ">> " --TODO (change)
    input <- getLine
    let (fs', output) = processInput fs input
    putStrLn output
    -- print fs' DEBUG
    replLoop fs'
        

main :: IO ()
main = do
    putStrLn "Lambda Calc REPL"
    replLoop Map.empty