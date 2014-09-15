module LogicProver.Lang where

-- The type of propositions
data Prop = PVar String
          | PNegate Prop
          | PAnd Prop Prop
          | POr Prop Prop
          | PCond Prop Prop
          deriving (Show, Eq)
          -- need to add biconditional

-- Extract the variable string from an atomic proposition
getVar :: Prop -> String
getVar (PVar p) = p
getVar (PNegate (PVar p)) = p
getVar _ = ""
