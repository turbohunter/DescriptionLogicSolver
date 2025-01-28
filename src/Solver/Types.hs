module Solver.Types
(
    Concept (..),
    Role (..),
    EdgeType (..),
    TableauNode (..),
    TableauTree (..),
    TableauZipper,
    -- Funktionen exportieren
    makeZipper,
    goUp,
    goDown,
    goRightSibling,
    addChild,
    nextNodeId,
    makeNode,
    getCurrentNode,
    setClash,
    toUnicode,
    toUnicodeList,
    printTableauTree
) where

import Data.List (intercalate) -- for pretty printing https://hackage.haskell.org/package/base-4.21.0.0/docs/Data-List.html#v:intercalate

data Role = Role {
    name :: String,
    id :: Int
} deriving(Show, Eq, Ord)

data Concept =
    Top                    
  | Bottom                 
  | Atom String           
  | Not Concept           
  | And Concept Concept    
  | Or Concept Concept     
  | Exists Role Concept    
  | Forall Role Concept    
  deriving (Show, Eq, Ord)

data EdgeType = 
    OrEdge           
  | RoleEdge Role    
  deriving (Show, Eq)

data TableauNode = TableauNode {
    nodeId :: Int,
    constraints :: [Concept],
    originalConstraints :: [Concept],
    hasClash :: Bool,
    edgeType :: Maybe EdgeType 
} deriving (Show, Eq)

data TableauTree = 
    Empty
  | Node TableauNode [TableauTree]  
  deriving (Eq)

-- [parentnode, left following tree , right follwing tree] basically a trail for navigation
type Path = [(TableauNode, [TableauTree], [TableauTree])]
type TableauZipper = (TableauTree, Path)

-- basic Zipper operations - https://wiki.haskell.org/index.php?title=Zipper  https://learnyouahaskell.com/zippers
makeZipper :: TableauTree -> TableauZipper
makeZipper t = (t, [])

goUp :: TableauZipper -> Maybe TableauZipper
goUp (_, []) = Nothing
goUp (t, (parent, ls, rs):ps) = 
    Just (Node parent (ls ++ [t] ++ rs), ps)

goDown :: TableauZipper -> Maybe TableauZipper
goDown (Empty, _) = Nothing
goDown (Node _ [], _) = Nothing
goDown (Node n (t:ts), ps) = 
    Just (t, (n, [], ts):ps)


-- Move focus to the right sibling
goRightSibling :: TableauZipper -> Maybe TableauZipper
goRightSibling (_t, []) =
    -- No parent means no siblings
    Nothing
goRightSibling (t, (parent, ls, r:rs) : ps) =
    -- The current node has a right sibling r.
    -- The new focus is r, and this node t moves to the left-siblings list.
    Just (r, (parent, ls ++ [t], rs) : ps)
goRightSibling (_, (_parent, _ls, []) : _) =
    -- No more siblings on the right
    Nothing


-- Get current node from zipper
getCurrentNode :: TableauZipper -> Maybe TableauNode
getCurrentNode (Node n _, _) = Just n
getCurrentNode (Empty, _) = Nothing

-- Set clash status for current node
setClash :: Bool -> TableauZipper -> TableauZipper
setClash clash (Node n children, path) = 
    (Node (n { hasClash = clash }) children, path)
setClash _ z@(Empty, _) = z

-- Create new node with given ID, constraints and edge type 
-- edgeType can be Nothing typically when it is root
makeNode :: Int -> [Concept] -> Maybe EdgeType -> TableauNode
makeNode nid concepts etype = TableauNode {
    nodeId = nid,
    constraints = concepts,
    originalConstraints = concepts,
    hasClash = False,
    edgeType = etype   
}

-- Add child node to current
addChild :: TableauNode -> TableauZipper -> TableauZipper
addChild newNode (Node n children, ps) = 
    (Node n (children ++ [Node newNode []]), ps)
addChild newNode (Empty, ps) = 
    (Node newNode [], ps)

-- Get next available node ID by scanning
nextNodeId :: TableauTree -> Int
nextNodeId Empty = 0
nextNodeId (Node node children) = 
    maximum (nodeId node : map nextNodeId children) + 1


-- | Convert a concept to Unicode representation
toUnicode :: Concept -> String
toUnicode = prettyPrint 0

toUnicodeList :: [Concept] -> String
toUnicodeList concepts = "[" ++  intercalate ", " (map toUnicode concepts) ++ "]"

-- | unicode print logicall expressions with precedence handling - windows powershell is not happy, probably needs better output.
-- | https://gitlab.haskell.org/ghc/ghc/-/issues/18307 
-- |  $OutputEncoding = [Console]::OutputEncoding = [Text.Encoding]::UTF8
-- |  chcp 65001
prettyPrint :: Int -> Concept -> String
prettyPrint _ Top = "⊤"
prettyPrint _ Bottom = "⊥"
prettyPrint _ (Atom atomName) = atomName
prettyPrint _ (Not c) = "¬" ++ prettyPrint 3 c
prettyPrint p (And c1 c2) = 
    parenthesize (p > 1) $ prettyPrint 1 c1 ++ " ⊓ " ++ prettyPrint 1 c2
prettyPrint p (Or c1 c2) = 
    parenthesize (p > 2) $ prettyPrint 2 c1 ++ " ⊔ " ++ prettyPrint 2 c2
prettyPrint _ (Exists role c) = 
    "∃" ++ name role ++ "." ++ prettyPrint 0 c
prettyPrint _ (Forall role c) = 
    "∀" ++ name role ++ "." ++ prettyPrint 0 c

-- | Add parentheses if needed based on precedence
parenthesize :: Bool -> String -> String
parenthesize True s = "(" ++ s ++ ")"
parenthesize False s = s



printTableauTree :: TableauTree -> String
printTableauTree = showTableauTree 0
    where
    showTableauTree :: Int -> TableauTree -> String
    showTableauTree _ Empty = "Empty"
    showTableauTree indent (Node node children) =
        indentStr ++ "Node " ++ show (nodeId node) ++ ":\n" ++
        indentStr ++ "  Constraints: " ++ toUnicodeList (originalConstraints node) ++ "\n" ++
        indentStr ++ "  Clash: " ++ show (hasClash node) ++ "\n" ++
        indentStr ++ "  EdgeType: " ++ show (edgeType node) ++ "\n" ++
        concatMap (showTableauTree (indent + 2)) children
        where
        indentStr = replicate indent ' ' -- https://hoogle.haskell.org/?hoogle=replicate
