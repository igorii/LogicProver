module LogicProver.Logic where

import Data.Map

-- The type of propositions
data Prop = PVar String
          | PNegate Prop
          | PAnd Prop Prop
          | POr Prop Prop
          | PCond Prop Prop
          deriving (Show, Eq)
          -- need to add biconditional

-- The type of the proof tree used for determining the validity of a proposition
data ProofTree = Branch2 { used :: Bool, prop :: Prop, left :: ProofTree, right :: ProofTree }
               | Branch1 { used :: Bool, prop :: Prop, left :: ProofTree }
               | Leaf    { used :: Bool, prop :: Prop }
               deriving (Show, Eq)

-- Extract the variable string from an atomic proposition
getVar :: Prop -> String
getVar (PVar p) = p
getVar (PNegate (PVar p)) = p
getVar _ = ""

-- Return true if the proposition is valid: there is some combination of truth
-- values for all of the atomic variables that allow the proposition to be true.
isValid :: Prop -> Bool
isValid = isConsitent . consitenctList . solveProp

-- Return true if all variables are consitent
isConsitent :: [(String, Bool)] -> Bool
isConsitent = all (\(k,v) -> v)

-- Create an association list of variable name to consitency
consitenctList :: ProofTree -> [(String, Bool)]
consitenctList t = Prelude.map gather $ toList $ getAtoms t where
    gather :: (String, [Prop]) -> (String, Bool)
    gather (k, l) = (k, all (\x -> x == head l) l)

-- Create a dictionary of variable names to atomic presence in the prooftree
getAtoms :: ProofTree -> Map String [Prop]
getAtoms t = getAtoms' t Data.Map.empty where
    getAtoms' t m = if isAtom t
        then let var = getVar $ prop t in
            case Data.Map.lookup var m of
                Nothing -> case t of
                    Leaf { used = _, prop = p } -> insert var [p] m
                    Branch1 { used = _, prop = p, left = l } -> getAtoms' l $ insert var [p] m
                    Branch2 { used = _, prop = p, left = l, right = r } ->
                        unionWith (++) (getAtoms' l $ insert var [p] m) (getAtoms' r $ insert var [p] m)
                Just past -> case t of
                    Leaf { used = _, prop = p } -> insert var (p:past) m
                    Branch1 { used = _, prop = p, left = l } -> getAtoms' l $ insert var (p:past) m
                    Branch2 { used = _, prop = p, left = l, right = r } ->
                        unionWith (++) (getAtoms' l $ insert var (p:past) m) (getAtoms' r $ insert var (p:past) m)
        else case t of 
            Leaf { used = _, prop = p } -> m
            Branch1 { used = _, prop = p, left = l } -> getAtoms' l m
            Branch2 { used = _, prop = p, left = l, right = r } -> unionWith (++) (getAtoms' l m) (getAtoms' r m)

-- Apply a function to each leaf of a proof tree
morphLeaves :: (Prop -> ProofTree -> ProofTree) -> Prop -> ProofTree -> ProofTree
morphLeaves f p t = case t of 
    Leaf { used = _, prop = _ } -> f p t
    Branch1 { used = u, prop = p', left = l } ->
        Branch1 { used = u, prop = p', left = morphLeaves f p l }
    Branch2 { used = u, prop = p', left = l, right = r } ->
        Branch2 { used = u, prop = p', left = morphLeaves f p l, right = morphLeaves f p r}

-- 1) Take a proof tree
-- 2) Traverse the tree looking for the highest node that has not had a rule applied to it
-- 3) Construct the proof tree resulting from applying the found rule to the given tree
-- 4) Return the tree
step :: ProofTree -> ProofTree
step t = case used t of

    -- This branch has been used, so step its children if applicable
    True -> case t of
        Leaf { used = _, prop = _ } ->
            t
        Branch2 {used = _, prop = p, left = l, right = r} ->
            Branch2 { used  = True , prop  = p , left  = step l , right = step r }
        Branch1 { used = _, prop = p, left = l } ->
            Branch1 { used = True , prop = p , left = step l }

    -- Otherwise
    False -> case isAtom t of

        -- If atomic, there are no rules to apply, so mark it as hit
        True -> setUsed t

        -- If not, apply the rule associated with its proposition
        False -> case t of
            Leaf { used = u, prop = p } ->
                setUsed $ morphLeaves propToTree p t
            Branch1 { used = u, prop = p, left = l } ->
                setUsed $ morphLeaves propToTree p t
            Branch2 { used = u, prop = p, left = l, right = r } ->
                setUsed $ morphLeaves propToTree p t

-- Turn a proposition into a prooftree and apply all rules to it
solveProp :: Prop -> ProofTree
solveProp = solveTree . initTree

-- Given a proof tree, proceed to iteratively apply all rules to it until there
-- are no more rules to apply
solveTree :: ProofTree -> ProofTree
solveTree t = case treeSolved t of
    True -> t
    False -> solveTree $ step t

-- Returns true if the tree has been fully applied, false otherwise
treeSolved :: ProofTree -> Bool
treeSolved t = case t of
    Leaf { used = u, prop = _ } -> u
    Branch1 { used = u, prop = _, left = l } -> u && treeSolved l
    Branch2 { used = u, prop = _, left = l, right = r } -> u && treeSolved l && treeSolved r

-- Given a proposition and a prooftree, apply the rule of the proposition on a 
-- new proof tree that has as its root the given proposition
propToTree :: Prop -> ProofTree -> ProofTree

-- (not (not P))
propToTree p'@(PNegate (PNegate _)) (Leaf {used = u, prop = p }) =
    Branch1 { used = u , prop = p
            , left = Leaf { used = False, prop = collapseNegations p'} }

-- P /\ Q
propToTree (PAnd p1 p2) (Leaf {used = u, prop = p }) =
    Branch1 { used = u , prop = p
            , left = Branch1 { used = False , prop = p1
                             , left = Leaf { used = False , prop = p2 } } }

-- ~(P /\ Q)
propToTree (PNegate (PAnd p1 p2)) l =
    propToTree (POr (PNegate p1) (PNegate p2)) l

-- (P \/ Q)
propToTree (POr p1 p2) (Leaf {used = u, prop = p }) =
    Branch2 { used  = u
            , prop  = p
            , left  = Leaf { used = False, prop = p1 }
            , right = Leaf { used = False, prop = p2 }
            }

-- ~(P \/ Q)
propToTree (PNegate (POr p1 p2)) l =
    propToTree (PAnd (PNegate p1) (PNegate p2)) l

-- Collapse all stacked negations
collapseNegations :: Prop -> Prop
collapseNegations (PNegate (PNegate p)) = collapseNegations p
collapseNegations p = p

-- Return true is there is no rule to apply on the given node
isAtom :: ProofTree -> Bool
isAtom t = case prop t of
    PVar _ -> True
    PNegate (PVar _) -> True
    _ -> False

-- Given a propsition, create a proof tree by negating the proposition. This is
-- to facilitate a proof by contradication of the validity of the proposition.
initTree :: Prop -> ProofTree
initTree p = Leaf False (PNegate p)

-- Set the `used` flag on a prooftree node
setUsed (Branch2 _ p b1 b2) = Branch2 True p b1 b2
setUsed (Branch1 _ p b) = Branch1 True p b
setUsed (Leaf _ p) = Leaf True p
