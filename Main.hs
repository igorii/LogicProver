module Main where

import LogicProver.Logic
import Data.Map

iprop = POr (PVar "p") (PAnd (PVar "p") (PVar "q"))

main :: IO ()
main = putStrLn . show $ Data.Map.toList $ getAtoms $ solveProp iprop
