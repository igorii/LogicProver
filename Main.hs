module Main where

import LogicProver.Logic

itree = initTree (POr (PVar "p") (PAnd (PVar "p") (PVar "q")))

main :: IO ()
main = putStrLn . show $ solveTree itree
