module Solver.Parser
(
    parseInput,
    ParseError(..)
) where

import Solver.Types hiding (name)
import Data.Char (isUpper, isLower, isAlphaNum, isAlpha)

-- Custom Errors 
data ParseError = 
    InvalidConceptName String
  | InvalidRoleName String
  | UnbalancedParentheses
  | TrailingOperator
  | EmptyExpression
  | InvalidSyntax String
  deriving (Show, Eq)

-- Main parsing function (entry)
-- takes string input / returns parse error or concept 
parseInput :: String -> Either ParseError Concept
parseInput input = do
    let tokens = tokenize $ filter (not . (`elem` " ")) input
    case parse tokens of
        Left err -> Left err
        Right (remaining, result) -> 
            if null remaining
                then Right result
                else Left $ InvalidSyntax "Unexpected trailing characters"

-- Tokenization
data Token = 
    TIdentifier String
  | TOperator String
  | TLeftParen
  | TRightParen
  | TDot
  | TQuantifier Char
  deriving (Show, Eq)

-- convert input string into tokens
tokenize :: String -> [Token]
tokenize [] = []
tokenize ('(':xs) = TLeftParen : tokenize xs
tokenize (')':xs) = TRightParen : tokenize xs
tokenize ('.':xs) = TDot : tokenize xs
tokenize (x:xs)
-- handle E
    | x == 'E' && not (null xs) && (head xs == '.' || isLower (head xs)) = 
        TQuantifier 'E' : tokenize xs
     -- handle A
    | x == 'A' && not (null xs) && (head xs == '.' || isLower (head xs)) = 
        TQuantifier 'A' : tokenize xs
    | x == '&' = TOperator "&" : tokenize xs
    | x == '|' = TOperator "|" : tokenize xs
    | x == '!' = TOperator "!" : tokenize xs
    -- handle identifiers
    | isIdentifierStart x = 
        let (ident, rest) = span isIdentifierChar xs
        in TIdentifier (x:ident) : tokenize rest
    | otherwise = tokenize xs

-- type alias for parser
type Parser = [Token] -> Either ParseError ([Token], Concept)

-- main parser with lowest precedence operator 
parse :: Parser
parse = parseOr

-- parse or operator
parseOr :: Parser
parseOr tokens = do
    (remaining, left) <- parseAnd tokens
    case remaining of
        (TOperator "|":rest) -> do
            (remaining', right) <- parseAnd rest
            Right (remaining', Or left right)
        _ -> Right (remaining, left)

--parse and operator
parseAnd :: Parser
parseAnd tokens = do
    (remaining, left) <- parseUnary tokens
    case remaining of
        (TOperator "&":rest) -> do
            (remaining', right) <- parseUnary rest
            Right (remaining', And left right)
        _ -> Right (remaining, left)

-- NOT parser
parseUnary :: Parser
parseUnary [] = Left EmptyExpression
parseUnary (TOperator "!":rest) = do
    (remaining, expr) <- parseAtom rest
    Right (remaining, Not expr)
parseUnary tokens = parseAtom tokens

-- Parses atomic expressions (parentheses, quantifiers, identifiers)
parseAtom :: Parser
parseAtom [] = Left EmptyExpression
parseAtom (TLeftParen:rest) = do
    (remaining, expr) <- parse rest
    case remaining of
        (TRightParen:rest') -> Right (rest', expr)
        _ -> Left UnbalancedParentheses
parseAtom (TQuantifier q:TIdentifier roleName:TDot:rest)
    | isLower (head roleName) = do
        (remaining, concept) <- parse rest
        let constructor = if q == 'E' then Exists else Forall
        Right (remaining, constructor (Role roleName 1) concept)
    | otherwise = Left $ InvalidRoleName roleName
parseAtom (TIdentifier ident:rest)
    | ident == "TOP" = Right (rest, Top)
    | ident == "BOTTOM" = Right (rest, Bottom)
    | isUpper (head ident) = Right (rest, Atom ident)
    | otherwise = Left $ InvalidConceptName ident
parseAtom tokens = Left $ InvalidSyntax $ "Unexpected tokens: " ++ show tokens

-- helper - check if char (a-z A-Z = true)
isIdentifierStart :: Char -> Bool
isIdentifierStart c = isAlpha c

-- helper - check if char (a-z A-Z 0-9 _ = true)
isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '_'