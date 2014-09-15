module Main where

import LogicProver.Logic

validProp = POr (PVar "p") (PAnd (PVar "p") (PVar "q"))
invalidProp = PAnd (PVar "p") (PNegate (PVar "p"))

main :: IO ()
main = putStrLn . show $ (True, False) == (isValid validProp, isValid invalidProp)
