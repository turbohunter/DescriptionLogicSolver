module Main where

import Solver.Types
import Solver.NNF
import Solver.TableauSolver
import qualified TypesSpec
import Solver.Parser
import Control.Exception (catch, SomeException)
import System.IO (hFlush, stdout)
import Test.Hspec

-- Helper to safely print results.
safePrint :: Show a => String -> a -> IO ()
safePrint label x = do
    putStrLn $ "\nTrying to print: " ++ label
    catch (print x) (\e -> putStrLn $ "Error printing: " ++ show (e :: SomeException))
    putStrLn "Print completed"

-- Helper to test clash detection
testClash :: String -> TableauNode -> IO ()
testClash label node = do
    putStrLn $ "\nTesting clash detection for " ++ label
    print $ "Node constraints: " ++ show (constraints node)
    print $ "Has clash: " ++ show (hasNodeClash node)

-- | Handle user input and display results
handleUserInput :: String -> IO ()
handleUserInput input = do
    case parseInput input of
        Left err -> putStrLn $ "Invalid: " ++ show err
        Right concept -> do
            putStrLn "Valid expression. Processing..."
            putStrLn "\nOriginal concept:"
            print concept
            putStrLn "Original in Unicode:"
            putStrLn $ toUnicode concept

            let nnfForm = toNNF concept
            putStrLn "\nNNF form:"
            print nnfForm
            putStrLn "NNF in Unicode:"
            putStrLn $ toUnicode nnfForm

            -- Ausgabe der negierten NNF

            putStrLn "\nSatisfiability check with nnFormula:"
            let tree = buildTableau nnfForm
            let isSat = tableauSolverTree tree
            putStrLn $ "Result: " ++ show isSat
            putStrLn $ if isSat
                then "The original concept is correct."
                else "The original concept is incorrect."
            putStrLn "\nTableau structure:"
            putStrLn $ printTableauTree tree

-- | Display help information
displayHelp :: IO ()
displayHelp = do
    putStrLn "\nDescription Logic Solver - Help"
    putStrLn "================================"
    putStrLn "Syntax:"
    putStrLn "  - Basic operators: & (AND), | (OR), ! (NOT)"
    putStrLn "  - Quantifiers: E (EXISTS), A (FORALL)"
    putStrLn "  - Special concepts: TOP, BOTTOM"
    putStrLn "  - Concepts must start with uppercase letters"
    putStrLn "  - Roles must start with lowercase letters"
    putStrLn "\nExamples:"
    putStrLn "  Person & !Robot"
    putStrLn "  E hasChild.Doctor"
    putStrLn "  A hasParent.Human"
    putStrLn "  Student & (E takes.Course)"
    putStrLn "  !(A & B) | (E hasChild.(C & A hasParent.TOP)) & BOTTOM"
    putStrLn "\nCommands:"
    putStrLn "  :help  - Show this help"
    putStrLn "  :quit  - Exit the program"
    putStrLn "  :test  - Run test cases"

-- | Interactive command loop
interactiveLoop :: IO ()
interactiveLoop = do
    putStr "> "
    hFlush stdout  -- Ensure prompt is displayed
    line <- getLine
    case line of
        ":quit" -> putStrLn "Servus!"
        ":help" -> displayHelp >> interactiveLoop
        ":test" -> runTests >> interactiveLoop
        "" -> interactiveLoop
        _ -> handleUserInput line >> interactiveLoop

-- | Run test cases
runTestsX :: IO ()
runTestsX = do
    putStrLn "\n=== Formula Tests ==="
    let expr = And (Atom "A") (Or (Atom "B") (Not (Atom "C")))
    putStrLn "\nTesting formula:"
    safePrint "Original" expr
    safePrint "NNF form" $ toNNF expr
    let nnfExpr = toNNF expr
    safePrint "Result" $ tableauSolverConcept nnfExpr

runTests :: IO ()
runTests = hspec TypesSpec.spec

main :: IO ()
main = do
    putStrLn "\nDescription Logic Solver"
    putStrLn "====================="
    displayHelp
    putStrLn "\nEnter a concept expression, :help for syntax guide, or :quit to exit"
    putStrLn ""
    interactiveLoop

