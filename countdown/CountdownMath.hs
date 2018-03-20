-- module of functions that say whether the target number can be made from the inputs and only operators +-*/
-- inputs: up to 4 from [25, 50, 75, 100] and the rest from
-- [1, 1, 2 , 2 , 3 , 3 , 4 , 4 , 5 , 5 , 6 , 6 , 7 , 7 , 8 , 8 , 9 , 9 , 10 , 10] to make 6 numbers in total.
-- target is a random integer between [101, 999]
-- not all inputs need to be used
-- running total cannot be -ve or a fraction
module CountdownMath
( countdownRPN
) where

import Data.List

-- below is the RPN evaluator from Learn You A Haskell
solveRPN :: String -> Float
solveRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*" = (x * y):ys
          foldingFunction (x:y:ys) "+" = (x + y):ys
          foldingFunction (x:y:ys) "-" = (y - x):ys
          foldingFunction (x:y:ys) "/" = (y / x):ys
          foldingFunction (x:y:ys) "^" = (y ** x):ys
          foldingFunction (x:xs) "ln" = log x:xs
          foldingFunction xs "sum" = [sum xs]
          foldingFunction xs numberString = read numberString:xs

-- for our purposes we want just the 1st 4 operators, and we want our evaluation to fail if the value
-- on the top of the stack becomes either negative or a fraction
-- As our inputs for this are always from the lists above we do not need to worry about:
--   addition resulting in a negative
--   multiplication resulting in a negative
-- Therefore the calculations that can fail are:
--   division by zero (this is covered by not allowing subtraction of equal numbers)
--   division resulting in a fraction
--   subtraction resulting in a negative
countdownRPN :: String -> Maybe Int
countdownRPN = head . foldl foldingFunction [] . words
    where foldingFunction (x:y:ys) "*" = (x * y):ys
          foldingFunction (x:y:ys) "+" = (x + y):ys
          foldingFunction (x:y:ys) "-" = if y > x then Just (y - x):ys else Nothing
          foldingFunction (x:y:ys) "/" = if y `mod` x == 0 then Just (y / x):ys else Nothing
          foldingFunction xs numberString = read numberString:xs
          foldingFunction Nothing _ = Nothing

foldingFunction :: Maybe [Int] -> String -> Maybe [Int]

-- Axiom: all RPN strings can be rearranged to "number number ... operator operator..."

