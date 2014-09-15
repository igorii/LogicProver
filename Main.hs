module Main where

import LogicProver.Lang
import LogicProver.Prover

validProp = POr (PVar "p") (PNegate (PVar "p"))
invalidProp = PAnd (PVar "p") (PNegate (PVar "p"))

validProp2 = PCond (PVar "p") (PVar "p")
invalidProp2 = PCond (PVar "p") (PVar "q")

-- [(A implies B) and (A implies C)] implies [A implies (B and C)]
longValid = PCond (PAnd (PCond (PVar "A") (PVar "B")) (PCond (PVar "A") (PVar "C"))) 
    (PCond (PVar "A") (PAnd (PVar "B") (PVar "C")))

main :: IO ()
main = putStrLn . show $ [True, True, True, False, False] == 
    map isValid [longValid, validProp, validProp2, invalidProp, invalidProp2]
