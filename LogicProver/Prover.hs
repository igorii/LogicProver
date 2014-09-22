module LogicProver.Prover (isValid) where

import qualified Data.Map as M
import LogicProver.Lang

-- The type of the proof tree used for determining the validity of a proposition
data ProofTree = Leaf    Bool Prop                     -- used, prop
               | Branch1 Bool Prop ProofTree           -- used, prop, left
               | Branch2 Bool Prop ProofTree ProofTree -- used, prop, left, right
               deriving (Show, Eq)

getProp :: ProofTree -> Prop
getProp (Leaf    _ p)      = p
getProp (Branch1 _ p _)    = p
getProp (Branch2 _ p _ _)  = p

getUsed :: ProofTree -> Bool
getUsed (Leaf    u _)      = u
getUsed (Branch1 u _ _)    = u
getUsed (Branch2 u _ _ _)  = u

-- Return true if the proposition is valid: there is some combination of truth
-- values for all of the atomic variables that allow the proposition to be true.
isValid :: Prop -> Bool
isValid = allClosed . collapseBranches . openBranches . solveProp

-- Return true if all variables are consitent
isConsistent :: [(String, Bool)] -> Bool
isConsistent = all (\(k,v) -> v)

-- Takes a list of True for an open branch and False for a closed branch
allClosed :: [Bool]-> Bool
allClosed = all (\x -> not x)

-- Collapse a branch of the proof tree (a list of vars to consistency) into a
-- single consistency value.
collapseBranches :: [[(String, Bool)]] -> [Bool]
collapseBranches l = map isConsistent l

-- Create a list where each element in a branch of the proof tree. Each subelement
-- is a variable appearing in the branch and its consistency within the branch.
openBranches :: ProofTree -> [[(String, Bool)]]
openBranches t = map isOpenBranch $ getAtoms t

-- Create an association list of variable name to consitency
isOpenBranch :: M.Map String [Prop] -> [(String, Bool)]
isOpenBranch m = map gather $ M.toList $ m where
    gather :: (String, [Prop]) -> (String, Bool)
    gather (k, l) = (k, all (\x -> x == head l) l)

-- Create a dictionary of variable names to atomic presence in the prooftree
getAtoms :: ProofTree -> [ M.Map String [Prop] ]
getAtoms t = getAtoms' t M.empty where
    getAtoms' t m = if isAtom t

        -- If the current node is an atom, then add it to the dictionary
        then let var = getVar $ getProp t in
            case M.lookup var m of

                -- If it does not exist in the dictionary, add it
                Nothing -> case t of
                    Leaf    _ p     -> (M.insert var [p] m) : []
                    Branch1 _ p l   -> getAtoms' l $ M.insert var [p] m
                    Branch2 _ p l r -> (getAtoms' l $ M.insert var [p] m) ++ (getAtoms' r $ M.insert var [p] m)

                -- Otherwise, append it to the current entry for the variable
                Just past -> case t of
                    Leaf    _ p     -> (M.insert var (p:past) m) : []
                    Branch1 _ p l   -> getAtoms' l $ M.insert var (p:past) m
                    Branch2 _ p l r -> (getAtoms' l $ M.insert var (p:past) m) ++ (getAtoms' r $ M.insert var (p:past) m)

        -- If the current node is not atomic, skip the entry and continue
        else case t of 
            Leaf    _ p     -> [m]
            Branch1 _ p l   -> getAtoms' l m
            Branch2 _ p l r -> (getAtoms' l m) ++ (getAtoms' r m)

-- Apply a function to each leaf of a proof tree
morphLeaves :: (Prop -> ProofTree -> ProofTree) -> Prop -> ProofTree -> ProofTree
morphLeaves f p t = case t of 
    Leaf _ _         -> f p t
    Branch1 u p' l   -> Branch1 u p' (morphLeaves f p l)
    Branch2 u p' l r -> Branch2 u p' (morphLeaves f p l) (morphLeaves f p r)

-- 1) Take a proof tree
-- 2) Traverse the tree looking for the highest node that has not had a rule applied to it
-- 3) Construct the proof tree resulting from applying the found rule to the given tree
-- 4) Return the tree
step :: ProofTree -> ProofTree
step t = case getUsed t of

    -- This branch has been used, so step its children if applicable
    True -> case t of
        Leaf _ _        -> t
        Branch1 _ p l   -> Branch1 True p (step l)
        Branch2 _ p l r -> Branch2 True p (step l) (step r)

    -- Otherwise
        -- If atomic, there are no rules to apply, so mark it as hit
        -- If not, apply the rule associated with its proposition
    False -> case isAtom t of
        True  -> setUsed t
        False -> case t of
            Leaf    u p     -> setUsed $ morphLeaves propToTree p t
            Branch1 u p l   -> setUsed $ morphLeaves propToTree p t
            Branch2 u p l r -> setUsed $ morphLeaves propToTree p t

-- Turn a proposition into a prooftree and apply all rules to it
solveProp :: Prop -> ProofTree
solveProp = solveTree . initTree

-- Given a proof tree, proceed to iteratively apply all rules to it until there
-- are no more rules to apply
solveTree :: ProofTree -> ProofTree
solveTree t = case treeSolved t of
    True  -> t
    False -> solveTree $ step t

-- Returns true if the tree has been fully applied, false otherwise
treeSolved :: ProofTree -> Bool
treeSolved t = case t of
    Leaf    u _     -> u
    Branch1 u _ l   -> u && treeSolved l
    Branch2 u _ l r -> u && treeSolved l && treeSolved r

-- Given a proposition and a prooftree, apply the rule of the proposition on a 
-- new proof tree that has as its root the given proposition
propToTree :: Prop -> ProofTree -> ProofTree

-- (not (not P))
propToTree p'@(PNegate (PNegate _)) (Leaf u p) =
    Branch1 u p $ Leaf False (collapseNegations p')

-- P and Q
propToTree (PAnd p1 p2) (Leaf u p) =
    Branch1 u p $ Branch1 False p1 $ Leaf False p2

-- ~(P and Q)
propToTree (PNegate (PAnd p1 p2)) l =
    propToTree (POr (PNegate p1) (PNegate p2)) l

-- (P or Q)
propToTree (POr p1 p2) (Leaf u p) =
    Branch2 u p (Leaf False p1) (Leaf False p2)

-- ~(P or Q)
propToTree (PNegate (POr p1 p2)) l =
    propToTree (PAnd (PNegate p1) (PNegate p2)) l

-- P implies Q
propToTree (PCond p1 p2) l =
    propToTree (POr (PNegate p1) p2) l

-- ~(P implies Q)
propToTree (PNegate (PCond p1 p2)) l =
    propToTree (PAnd p1 (PNegate p2)) l

-- Collapse all stacked negations
collapseNegations :: Prop -> Prop
collapseNegations (PNegate (PNegate p)) = collapseNegations p
collapseNegations p = p

-- Return true is there is no rule to apply on the given node
isAtom :: ProofTree -> Bool
isAtom t = case getProp t of
    PVar _           -> True
    PNegate (PVar _) -> True
    _                -> False

-- Given a propsition, create a proof tree by negating the proposition. This is
-- to facilitate a proof by contradication of the validity of the proposition.
initTree :: Prop -> ProofTree
initTree p = Leaf False (PNegate p)

-- Set the `used` flag on a prooftree node
setUsed :: ProofTree -> ProofTree
setUsed (Leaf    _ p)       = Leaf True p
setUsed (Branch1 _ p b)     = Branch1 True p b
setUsed (Branch2 _ p b1 b2) = Branch2 True p b1 b2
