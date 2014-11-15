module Main where

import LogicProver.Lang
import LogicProver.Prover
import LogicProver.Parse

validProp = "p impl p"
main :: IO ()
main = putStrLn . show $ case parseString prop validProp of 
    Right x -> isValid x
    Left err -> False
