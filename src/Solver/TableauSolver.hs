module Solver.TableauSolver where

import Data.List (find, nub, partition)
import Solver.Types
  ( Concept (..),
    EdgeType (OrEdge, RoleEdge),
    TableauNode (constraints, edgeType, hasClash),
    TableauTree (..),
    TableauZipper,
    addChild,
    getCurrentNode,
    goDown,
    goRightSibling,
    goUp,
    makeNode,
    makeZipper,
    nextNodeId,
  )

-- | Main function to check if a formula is satisfiable. Returns True if no clash was detected.
tableauSolverConcept :: Concept -> Bool
tableauSolverConcept formula =
  let tree = buildTableau formula
   in not (hasTreeClash tree)

-- | Checks whether a tableau tree is satisfiable by verifying the absence of clashes.
tableauSolverTree :: TableauTree -> Bool
tableauSolverTree tree = not $ hasTreeClash tree

-- | Builds the complete tableau tree starting from a root formula.
buildTableau :: Concept -> TableauTree
buildTableau formula =
  let rootNode = makeNode 0 [formula] Nothing
      zipper = makeZipper (Node rootNode [])
      result = buildTableauRec zipper
   in markClashes $ fst result

-- | Recursively builds the tableau tree by expanding nodes and traversing children and siblings.
-- (recursion, pattern matching)
buildTableauRec :: TableauZipper -> TableauZipper
buildTableauRec z@(Empty, _) = z
buildTableauRec z@((Node _ _), _) =
  case getCurrentNode z of
    Nothing -> z
    Just node ->
      if hasPathCycle z || isSaturated node
        then
          -- If the current node is done, move up or to siblings.
          case goRightSibling z of
            Just rs -> buildTableauRec rs
            Nothing ->
              case goUp z of
                Just up -> buildTableauRec up
                Nothing -> z
        else
          let zExpanded = expandNode z
           in case goDown zExpanded of
                Just downZ ->
                  -- Process child subtree recursively.
                  let childDone = buildTableauRec downZ
                   in case goRightSibling childDone of
                        Just rs -> buildTableauRec rs
                        Nothing ->
                          -- Done with children, move up.
                          case goUp childDone of
                            Just up -> buildTableauRec up
                            Nothing -> childDone
                Nothing ->
                  -- No child, try next sibling.
                  case goRightSibling zExpanded of
                    Just rs -> buildTableauRec rs
                    Nothing ->
                      case goUp zExpanded of
                        Just up -> buildTableauRec up
                        Nothing -> zExpanded

-- | Checks if there is a cycle in the path of the tableau (important for detecting infinite loops).
-- (list operations, lambda expressions)
hasPathCycle :: TableauZipper -> Bool
hasPathCycle (_, path) =
  let nodes = map (\(n, _, _) -> constraints n) path -- Extract constraints from nodes in the path.
   in any (\n -> length (filter (== n) nodes) > 1) nodes -- Check if any node appears more than once.

-- | Checks if a node is saturated, meaning no more rules can be applied.
-- Saturation occurs if all constraints are atomic and no AND, OR, or role formulas remain.
-- (higher-order functions, recursion)
isSaturated :: TableauNode -> Bool
isSaturated node =
  all isAtomic (constraints node)
    && not (any isAndFormula (constraints node))
    && not (any isOrFormula (constraints node))
    && not (any isRoleFormula (constraints node))
  where
    -- Determines if a formula is atomic.
    isAtomic (Atom _) = True
    isAtomic (Not (Atom _)) = True
    isAtomic Top = True
    isAtomic Bottom = True
    isAtomic _ = False

    -- Determines if a formula is an AND formula.
    isAndFormula (And _ _) = True
    isAndFormula _ = False

    -- Determines if a formula is an OR formula.
    isOrFormula (Or _ _) = True
    isOrFormula _ = False

    -- Determines if a formula is a role formula (Exists or Forall).
    isRoleFormula (Exists _ _) = True
    isRoleFormula (Forall _ _) = True
    isRoleFormula _ = False

-- | Higher-order function that applies a list of functions to a zipper.
-- (higher-order functions)
applyFunctionsToZipper :: [TableauZipper -> TableauZipper] -> TableauZipper -> TableauZipper
applyFunctionsToZipper [] zipper = zipper
applyFunctionsToZipper (f : fs) zipper = applyFunctionsToZipper fs (f zipper)

-- | Expands a node by applying tableau rules.
-- (higher-order functions, recursion)
expandNode :: TableauZipper -> TableauZipper
expandNode zipper =
  case getCurrentNode zipper of
    Nothing -> zipper
    Just _ -> applyFunctionsToZipper [applyAndRules, applyOrRules, applyRoleRules] zipper


-- | Checks if the entire tableau tree has a clash.
hasTreeClash :: TableauTree -> Bool
hasTreeClash Empty = False
hasTreeClash (Node node _) = hasClash node

-- | Checks if a node contains a clash by evaluating its constraints.
-- (list comprehensions, pattern matching)
hasNodeClash :: TableauNode -> Bool
hasNodeClash node =
  let cs = constraints node
      atomPairs = [(a, b) | a <- cs, b <- cs, a < b] -- Generate pairs of constraints without repetition.
   in any (uncurry isClashingPair) atomPairs || any isBottom cs
  where
    -- Checks if two concepts are clashing pairs.
    isClashingPair :: Concept -> Concept -> Bool
    isClashingPair (Atom a) (Not (Atom b)) = a == b
    isClashingPair (Not (Atom a)) (Atom b) = a == b
    isClashingPair _ _ = False

    -- Checks if a concept is Bottom (false).
    isBottom Bottom = True
    isBottom _ = False

-- | Marks clashes in a tableau tree by checking nodes and child relationships.
-- (recursion, pattern matching)
markClashes :: TableauTree -> TableauTree
markClashes Empty = Empty
markClashes (Node n children) =
  let markedChildren = map markClashes children
      nodeHasLocalClash = hasNodeClash n
      (orChildren, roleChildren) = partition isOrChild markedChildren
      orClashes = if null orChildren then False else all hasTreeClash orChildren
      roleClashes = any hasTreeClash roleChildren
      nodeClashes = nodeHasLocalClash || orClashes || roleClashes
      newNode = n {hasClash = nodeClashes}
   in Node newNode markedChildren
  where
    isOrChild :: TableauTree -> Bool
    isOrChild Empty = False
    isOrChild (Node childNode _) =
      case edgeType childNode of
        Just OrEdge -> True
        _ -> False


-- | Applies AND rules until no more are applicable.
-- (recursion, list operations)
applyAndRules :: TableauZipper -> TableauZipper
applyAndRules zipper =
  case getCurrentNode zipper of
    Nothing -> zipper
    Just node ->
      case find isAndFormula (constraints node) of
        Nothing -> zipper
        Just (And c1 c2) ->
          let newConstraints = nub $ c1 : c2 : filter (/= And c1 c2) (constraints node)
              newNode = node {constraints = newConstraints}
           in applyAndRules $ modifyCurrentNode newNode zipper
        Just _ -> zipper
  where
    isAndFormula (And _ _) = True
    isAndFormula _ = False

-- | Flattens nested OR structures into a list of concepts.
-- (recursion)
flattenOr :: Concept -> [Concept]
flattenOr (Or c1 c2) = flattenOr c1 ++ flattenOr c2
flattenOr c = [c]

-- | Applies OR rules by creating child nodes for each disjunction component.
-- (recursion, list operations)
applyOrRules :: TableauZipper -> TableauZipper
applyOrRules zipper =
  case getCurrentNode zipper of
    Nothing -> zipper
    Just node ->
      case find isOrFormula (constraints node) of
        Nothing -> zipper
        Just orConstraint ->
          let subConcepts = flattenOr orConstraint
              currentConstraints = filter (/= orConstraint) (constraints node)
              newNode = node {constraints = currentConstraints}
              updatedZipper = modifyCurrentNode newNode zipper
              addAllChildren z [] = z
              addAllChildren z (c : cs) =
                let child =
                      makeNode
                        (nextNodeId (fst z))
                        (nub (c : currentConstraints))
                        (Just OrEdge)
                    z' = addChild child z
                 in addAllChildren z' cs
              finalZipper = addAllChildren updatedZipper subConcepts
           in finalZipper
  where
    isOrFormula (Or _ _) = True
    isOrFormula _ = False

-- | Applies role rules (Exists and Forall) by creating child nodes.
-- (recursion, list operations)
applyRoleRules :: TableauZipper -> TableauZipper
applyRoleRules zipper =
  case getCurrentNode zipper of
    Nothing -> zipper
    Just node ->
      case find isRoleFormula (constraints node) of
        Nothing -> zipper
        Just (Exists role c) ->
          let newNode = makeNode (nextNodeId (fst zipper)) [c] (Just $ RoleEdge role)
              newConstraints = filter (/= Exists role c) (constraints node)
              updatedNode = node {constraints = newConstraints}
           in applyRoleRules $ addChild newNode $ modifyCurrentNode updatedNode zipper
        Just (Forall role c) ->
          let newNode = makeNode (nextNodeId (fst zipper)) [c] (Just $ RoleEdge role)
              newConstraints = filter (/= Forall role c) (constraints node)
              updatedNode = node {constraints = newConstraints}
           in applyRoleRules $ addChild newNode $ modifyCurrentNode updatedNode zipper
        Just _ -> zipper
  where
    isRoleFormula (Exists _ _) = True
    isRoleFormula (Forall _ _) = True
    isRoleFormula _ = False

-- | Helper to modify the current node in a zipper.
-- (pattern matching)
modifyCurrentNode :: TableauNode -> TableauZipper -> TableauZipper
modifyCurrentNode newNode (Node _ children, path) = (Node newNode children, path)
modifyCurrentNode _ z@(Empty, _) = z
