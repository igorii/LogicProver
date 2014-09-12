module Main where

import LogicProver.Logic

itree = initTree (PAnd (PVar "p") (PVar "q"))

main :: IO ()
main = putStrLn . show $ step $ step itree
