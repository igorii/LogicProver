module Main where

import LogicProver.Lang
import LogicProver.Prover

validProp = POr (PVar "p") (PNegate (PVar "p"))
invalidProp = PAnd (PVar "p") (PNegate (PVar "p"))

validProp2 = PCond (PVar "p") (PVar "p")
invalidProp2 = PCond (PVar "p") (PVar "q")

main :: IO ()
main = putStrLn . show $ [True, True, False, False] == map isValid [validProp, validProp2, invalidProp, invalidProp2]
