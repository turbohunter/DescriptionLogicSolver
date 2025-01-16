# descriptiveLogicSolver
this repository is used for creating a description logic solver in haskell as part of a FWPM module at OTH Regensburg

## GitHub Action Pipeline
- we have a CI workflow executing ```cabal test``` 
- we have a hlint workflow performing linting of the code 
## debugging possibilities
Debugging in Haskell can be a bit different from debugging in other programming languages. Haskell is a functional language, which means that the code is based on the mathematical concept of functions. As a result, debugging in Haskell involves understanding the flow of functions and how they interact with each other.


Debug.Trace
One of the most popular debugging tools in Haskell is the Debug.Trace module. This module provides a simple way to print out information about your code as it runs.

For example, suppose you have a function that takes two parameters and returns their sum:
``` haskell
add :: Int -> Int -> Int
add x y = x + y
``` 
If you want to see the value of x and y as the function runs, you can add the following line to your code:
```  haskell
add :: Int -> Int -> Int
add x y = trace ("x = " ++ show x ++ ", y = " ++ show y) $ x + y
```
The trace function takes a string as an argument and prints it out to the console. In this example, we're printing out the values of x and y.

By using Debug.Trace, you can add print statements to your code to help you understand what's going on as your functions run.

## docu under the [Wiki tab](https://github.com/turbohunter/descriptiveLogicSolver/wiki)

