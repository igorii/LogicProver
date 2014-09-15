module Main where

import LogicProver.Logic
import Data.Map

iprop = POr (PVar "p") (PAnd (PVar "p") (PVar "q"))
iprop2 = PAnd (PVar "p") (PNegate (PVar "p"))

main :: IO ()
main = putStrLn . show $ isValid iprop
