module Main where

import System.IO
import Parser
import System.Environment
import Eval
import Control.Monad
import Data.Map (Map)
import qualified Data.Map as Map

defaultLibs :: [FilePath]
defaultLibs = ["lib/std.lambda", "lib/num.lambda"]

-- This is a State Monad
processInput :: FuncsMap -> String -> (FuncsMap, String)
processInput fs input = case runParser stmntP input of
    Just ("", result) -> case result of
        Define islazy name expr -> (Map.insert name (islazy, calcBruijn expr) fs, name ++ (if islazy then " := " else " = ") ++ show expr)
        Eval        expr -> (fs, show expr')
            where expr' = eval fs $ calcBruijn expr
    Just _  -> (fs, "ERROR: wrong input")
    Nothing -> (fs, "ERROR: wrong input")

sequenceInput_ :: FuncsMap -> [String] -> FuncsMap
sequenceInput_ = foldl (\ fs d -> fst $ processInput fs d)

parseStd :: IO FuncsMap
parseStd = do
    (stdlibs :: [FilePath]) <- getArgs
    decls <- mapM readFile (defaultLibs ++ stdlibs)
    return $ sequenceInput_ Map.empty (concatMap lines decls)


main :: IO ()
main = do
    putStrLn "Lambda Calc REPL"
    stdFuncs <- parseStd
    replLoop stdFuncs

replLoop :: FuncsMap -> IO ()
replLoop fs = do
    putStr "·> " --TODO (change)
    hFlush stdout
    input <- getLine
    let (fs', output) = processInput fs input
    putStrLn output
    replLoop fs'
