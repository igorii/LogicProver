module Main where

import System.Environment
import Data.List

import LogicProver.Lang
import LogicProver.Prover
import LogicProver.Parse

validProp = "p impl p"

eval str =
    show $ case parseString prop str of
        Right x  -> isValid x
        Left err -> False

main :: IO ()
main = do 
    ps <- getArgs
    mapM (\x -> putStrLn $ eval x) ps
    return ()
