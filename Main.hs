module Main where

import LogicProver.Logic

iprop = POr (PVar "p") (PAnd (PVar "p") (PVar "q"))

main :: IO ()
main = putStrLn . show $ solveProp iprop
