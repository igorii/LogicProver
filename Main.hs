module Main where

import LogicProver.Lang
import LogicProver.Prover

validProp = (PVar "p") `POr` (PNegate (PVar "p"))
invalidProp = (PVar "p") `PAnd` (PNegate (PVar "p"))

validProp2 = (PVar "p") `PCond` (PVar "p")
invalidProp2 = (PVar "p") `PCond` (PVar "q")

-- [(A implies B) and (A implies C)] implies [A implies (B and C)]
longValid = (((PVar "A") `PCond` (PVar "B")) `PAnd` ((PVar "A") `PCond` (PVar "C"))) `PCond`
    ((PVar "A") `PCond` ((PVar "B") `PAnd` (PVar "C")))

main :: IO ()
main = putStrLn . show $ [True, True, True, False, False] ==
    map isValid [longValid, validProp, validProp2, invalidProp, invalidProp2]
