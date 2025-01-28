module Solver.NNF
where
import Solver.Types

-- Formel negieren
negateFormula :: Concept -> Concept
negateFormula = Not

-- Umwandeln in NNF
toNNF :: Concept -> Concept
toNNF Top = Top 
toNNF Bottom = Bottom
toNNF (Atom a) = Atom a
toNNF (Not Top) = Bottom
toNNF (Not Bottom) = Top
toNNF (Not (Atom a)) = Not (Atom a)
toNNF (Not (Not a)) = toNNF a
toNNF (Not (And a b)) = Or (toNNF (Not a)) (toNNF (Not b)) 
toNNF (Not (Or a b)) = And (toNNF (Not a)) (toNNF (Not b)) 
toNNF (Not (Forall r c)) = Exists r (toNNF (Not c))
toNNF (Not (Exists r c)) = Forall r (toNNF (Not c))
toNNF (And a b) = And (toNNF a) (toNNF b) 
toNNF (Or a b) = Or (toNNF a) (toNNF b) 
toNNF (Forall r c) = Forall r (toNNF c) 
toNNF (Exists r c) = Exists r (toNNF c)

-- Formel negieren und in NNF überführen
negateAndConvertToNNF :: Concept -> Concept
negateAndConvertToNNF = toNNF . negateFormula
