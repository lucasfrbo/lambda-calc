module Main where

import Parser
import Eval
import Control.Monad
import Data.Map

processInput :: String -> String
processInput input = maybe "ERROR: Wrong input" f (run stmntP input)
    where
        f :: (String, Statement) -> String
        f (_, Eval expr) = (show . eval . calcBruijn) expr
        f (_, Define var expr) = undefined
-- TODO!!!


main :: IO ()
main = forever $ do
    input <- getLine
    putStrLn $ processInput input
    -- putStrLn $ maybe "ERROR: Wrong input" (show . snd) $ run stmntP input
    return ()