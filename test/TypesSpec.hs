module TypesSpec where

import Test.QuickCheck
import Test.Hspec
import Solver.Types
import Solver.TableauSolver
import Solver.NNF

-- Test Roles
testRole1 :: Role
testRole1 = Role "r" 1

testRole2 :: Role
testRole2 = Role "s" 2

-- Basic concept tests
prop_basicConcepts :: Bool
prop_basicConcepts = and [
    tableauSolverConcept (toNNF Top),
    not $ tableauSolverConcept (toNNF Bottom),
    tableauSolverConcept (toNNF (Atom "A")),
    tableauSolverConcept (toNNF (Not Bottom)),  -- wird zu Top
    not $ tableauSolverConcept (toNNF (And (Atom "A") (Not (Atom "A"))))
    ]

-- Fehlerhafte Basic Concepts
-- Test 1: ¬⊤ ≡ ⊥ (nach NNF)
-- Test 2: ⊤ ∧ ⊥ ≡ ⊥
-- Test 3: ¬⊥ ≡ ⊤ (nach NNF)
prop_invalidBasicConcepts :: Bool
prop_invalidBasicConcepts = and [
    not $ tableauSolverConcept Bottom,  -- direkt Bottom verwenden
    not $ tableauSolverConcept (And Top Bottom),
    tableauSolverConcept Top  -- direkt Top verwenden
    ]

-- IK Axioms Tests with φ = A, ψ = B
-- IK1: □(φ → ψ) ⊃ (□φ ⊃ □ψ)
-- ¬∀r.(¬φ ⊔ ψ) ⊔ ¬∀r.φ ⊔ ∀r.ψ
prop_IK1 :: Bool
prop_IK1 = tableauSolverConcept $ toNNF $
    Or (Not (Forall testRole1 (Or (Not (Atom "A")) (Atom "B"))))
       (Or (Not (Forall testRole1 (Atom "A")))
           (Forall testRole1 (Atom "B")))

-- IK2: □(φ → ψ) ⊃ (◇φ ⊃ ◇ψ)
-- ¬∀r.(¬φ ⊔ ψ) ⊔ ¬∃r.φ ⊔ ∃r.ψ
prop_IK2 :: Bool
prop_IK2 = tableauSolverConcept $ toNNF $
    Or (Not (Forall testRole1 (Or (Not (Atom "A")) (Atom "B"))))
       (Or (Not (Exists testRole1 (Atom "A")))
           (Exists testRole1 (Atom "B")))

-- IK3: ¬◇⊥
-- ¬∃r.⊥
prop_IK3 :: Bool
prop_IK3 = tableauSolverConcept $ toNNF $
    Not (Exists testRole1 Bottom)

-- IK4: ◇(φ ∨ ψ) ⊃ (◇φ ∨ ◇ψ)
-- ¬∃r.(φ ⊔ ψ) ⊔ (∃r.φ ⊔ ∃r.ψ)
prop_IK4 :: Bool
prop_IK4 = tableauSolverConcept $ toNNF $
    Or (Not (Exists testRole1 (Or (Atom "A") (Atom "B"))))
       (Or (Exists testRole1 (Atom "A"))
           (Exists testRole1 (Atom "B")))

-- IK5: (◇φ ⊃ □ψ) ⊃ □(φ ⊃ ψ)
-- ¬(¬∃r.φ ⊔ ∀r.ψ) ⊔ ∀r.(¬φ ⊔ ψ)
prop_IK5 :: Bool
prop_IK5 = tableauSolverConcept $ toNNF $
    Or (Not (Or (Not (Exists testRole1 (Atom "A")))
                (Forall testRole1 (Atom "B"))))
       (Forall testRole1 (Or (Not (Atom "A")) (Atom "B")))

prop_all_IK :: Bool
prop_all_IK = 
    let ik5 = Or (Not (Or (Not (Exists testRole1 (Atom "A")))
                (Forall testRole1 (Atom "B"))))
             (Forall testRole1 (Or (Not (Atom "A")) (Atom "B")))
        ik4 = Or (Not (Exists testRole1 (Or (Atom "A") (Atom "B"))))
            (Or (Exists testRole1 (Atom "A"))
                (Exists testRole1 (Atom "B")))

        ik3 = Not (Exists testRole1 Bottom)
        ik2 = Or (Not (Forall testRole1 (Or (Not (Atom "A")) (Atom "B"))))
            (Or (Not (Exists testRole1 (Atom "A")))
             (Exists testRole1 (Atom "B")))
        ik1 = Or (Not (Forall testRole1 (Or (Not (Atom "A")) (Atom "B"))))
            (Or (Not (Forall testRole1 (Atom "A")))
                (Forall testRole1 (Atom "B")))
        ik_all = And (And (And ik1 ik2) ik3) (And ik4 ik5)
     in tableauSolverConcept $ toNNF $ ik_all


-- FS Axioms Tests
-- FS1: □⊤
prop_FS1 :: Bool
prop_FS1 = tableauSolverConcept $ toNNF $
    Forall testRole1 Top

-- FS2: □(φ ∧ ψ) ≡ (□φ ∧ □ψ)
prop_FS2 :: Bool
prop_FS2 = tableauSolverConcept $ toNNF $
    And (Or (Not (Forall testRole1 (And (Atom "A") (Atom "B"))))
            (And (Forall testRole1 (Atom "A"))
                 (Forall testRole1 (Atom "B"))))
        (Or (Not (And (Forall testRole1 (Atom "A"))
                      (Forall testRole1 (Atom "B"))))
            (Forall testRole1 (And (Atom "A") (Atom "B"))))

-- FS3: Same as IK3
prop_FS3 :: Bool
prop_FS3 = prop_IK3

-- FS4: ◇(φ ∨ ψ) ≡ (◇φ ∨ ◇ψ)
prop_FS4 :: Bool
prop_FS4 = tableauSolverConcept $ toNNF $
    And (Or (Not (Exists testRole1 (Or (Atom "A") (Atom "B"))))
            (Or (Exists testRole1 (Atom "A"))
                (Exists testRole1 (Atom "B"))))
        (Or (Not (Or (Exists testRole1 (Atom "A"))
                     (Exists testRole1 (Atom "B"))))
            (Exists testRole1 (Or (Atom "A") (Atom "B"))))

-- FS5: Same as IK5
prop_FS5 :: Bool
prop_FS5 = prop_IK5

-- FS6: ◇(φ → ψ) ⊃ (□φ ⊃ ◇ψ)
prop_FS6 :: Bool
prop_FS6 = tableauSolverConcept $ toNNF $
    Or (Not (Exists testRole1 (Or (Not (Atom "A")) (Atom "B"))))
       (Or (Not (Forall testRole1 (Atom "A")))
           (Exists testRole1 (Atom "B")))

-- Complex Nested Tests
prop_complexNested1 :: Bool
prop_complexNested1 = tableauSolverConcept $ toNNF $
    Forall testRole1 (
        Or (Exists testRole2 (And (Atom "A") (Atom "B")))
           (And (Not (Atom "C"))
                (Forall testRole1 (Atom "D"))))

prop_complexNested2 :: Bool
prop_complexNested2 = tableauSolverConcept $ toNNF $
    And (Exists testRole1 (
            Or (Atom "A")
               (Exists testRole2 (Not (Atom "B")))))
        (Forall testRole2 (
            And (Atom "C")
                (Not (Exists testRole1 Bottom))))

-- Fehlerhafte komplexe Tests
-- Test 1: □(◇(A ∧ ⊥)) ∧ □D 
-- Dies ist unerfüllbar weil: A ∧ ⊥ ≡ ⊥, daher ◇(A ∧ ⊥) ≡ ◇⊥
prop_invalidComplexNested1 :: Bool
prop_invalidComplexNested1 = not $ tableauSolverConcept $ toNNF $
    And (Exists testRole2 (And (Atom "A") Bottom))
        (Forall testRole1 (Atom "D"))
-- Test 2: ◇⊥ ∧ ◇⊥
-- Dies ist unerfüllbar nach Axiom IK3: ¬◇⊥
prop_invalidComplexNested2 :: Bool
prop_invalidComplexNested2 = not $ tableauSolverConcept $ toNNF $
    And (Exists testRole1 Bottom)
        (Exists testRole2 Bottom)


-- Rules Tests
-- MP Rule: φ and φ → ψ implies ψ
prop_MPRule :: Bool
prop_MPRule = not $ tableauSolverConcept $ toNNF $
    And (Atom "A")
        (And (Or (Not (Atom "A")) (Atom "B"))
             (Not (Atom "B")))

-- Reg Rule: φ ⊃ ψ implies □φ ⊃ □ψ and ◇φ ⊃ ◇ψ
prop_RegRule :: Bool
prop_RegRule = tableauSolverConcept $ toNNF $
    And (Or (Not (Or (Not (Atom "A")) (Atom "B")))
            (Or (Not (Forall testRole1 (Atom "A")))
                (Forall testRole1 (Atom "B"))))
        (Or (Not (Or (Not (Atom "A")) (Atom "B")))
            (Or (Not (Exists testRole1 (Atom "A")))
                (Exists testRole1 (Atom "B"))))

-- Fehlerhafte Regeln
-- Test 1: A ∧ ¬A
-- Klassischer Widerspruch: φ ∧ ¬φ ≡ ⊥
prop_invalidMPRule :: Bool
prop_invalidMPRule = not $ tableauSolverConcept $ toNNF $
    And (Atom "A") 
        (Not (Atom "A"))

-- Test 2: ◇⊥ ∧ ◇⊥
-- Unerfüllbar nach IK3 (¬◇⊥), da ◇⊥ ∧ ◇⊥ ⊃ ◇⊥
prop_invalidRegRule :: Bool
prop_invalidRegRule = not $ tableauSolverConcept $ toNNF $
    And (Exists testRole1 Bottom)
        (Exists testRole1 Bottom)

prop_orBranchClash :: Bool
prop_orBranchClash = not $ tableauSolverConcept $ toNNF $
    Or (And (Atom "A") (Not (Atom "A")))
       (And (Atom "B") (Not (Atom "B")))

-- Should be satisfiable (only one branch has clash)
prop_orBranchPartialClash :: Bool
prop_orBranchPartialClash = tableauSolverConcept $ toNNF $
    Or (And (Atom "A") (Not (Atom "A")))
       (Atom "B")

-- Should be unsatisfiable (both branches have clashes)
prop_orBranchDoubleClash :: Bool
prop_orBranchDoubleClash = not $ tableauSolverConcept $ toNNF $
    And (Or (Atom "A") (Atom "B"))
        (And (Not (Atom "A")) (Not (Atom "B")))


spec :: Spec 
spec = do 
    describe "Basic Concepts" $ do
        it "should correctly evaluate basic concepts" $
            property prop_basicConcepts
        it "should correctly evaluate invalid basic concepts" $
            property prop_invalidBasicConcepts

    describe "IK Axioms" $ do
        it "should verify IK1" $
            property prop_IK1
        it "should verify IK2" $
            property prop_IK2
        it "should verify IK3" $
            property prop_IK3
        it "should verify IK4" $
            property prop_IK4
        it "should verify IK5" $
            property prop_IK5
        it "should verify all IK together" $ 
            property prop_all_IK

    describe "FS Axioms" $ do
        it "should verify FS1" $
            property prop_FS1
        it "should verify FS2" $
            property prop_FS2
        it "should verify FS3" $
            property prop_FS3
        it "should verify FS4" $
            property prop_FS4
        it "should verify FS5" $
            property prop_FS5
        it "should verify FS6" $
            property prop_FS6

    describe "Complex Nested Formulas" $ do
        it "should verify valid complex nested formula 1" $
            property prop_complexNested1
        it "should verify valid complex nested formula 2" $
            property prop_complexNested2
        it "should detect invalid complex nested formula 1" $
            property prop_invalidComplexNested1
        it "should detect invalid complex nested formula 2" $
            property prop_invalidComplexNested2

    describe "Logical Rules" $ do
        it "should verify valid MP rule" $
            property prop_MPRule
        it "should verify valid Reg rule" $
            property prop_RegRule
        it "should detect invalid MP rule" $
            property prop_invalidMPRule
        it "should detect invalid Reg rule" $ 
            property prop_invalidRegRule

    describe "Or-fix" $ do
        it "should verify valid orBranchClash rule" $
            property prop_orBranchClash
        it "should verify valid orBranchPartialClash rule" $
            property prop_orBranchPartialClash
        it "should verify valid orBranchDoubleClash rule" $
            property prop_orBranchDoubleClash
main :: IO ()
main = hspec $ do
    spec