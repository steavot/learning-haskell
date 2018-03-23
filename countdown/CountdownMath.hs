-- module of functions that say whether the target number can be made from the inputs and only operators +-*/
-- inputs: up to 4 from [25, 50, 75, 100] and the rest from
-- [1, 1, 2 , 2 , 3 , 3 , 4 , 4 , 5 , 5 , 6 , 6 , 7 , 7 , 8 , 8 , 9 , 9 , 10 , 10] to make 6 numbers in total.
-- target is a random integer between [101, 999]
-- not all inputs need to be used
-- running total cannot be -ve or a fraction
module CountdownMath
( countdownRPN
, operatorPerms
, tilePerms
, rpnPermutations
, allRPNs
, getASolution
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
countdownRPN = fmap head . (foldl foldingFunction (Just [])) . words
    where foldingFunction :: Maybe [Int] -> String -> Maybe [Int]
          foldingFunction (Just (x:y:ys)) "*" = Just ((x * y):ys)
          foldingFunction (Just (x:y:ys)) "+" = Just ((x + y):ys)
          foldingFunction (Just (x:y:ys)) "-" = if y > x then Just ((y - x):ys) else Nothing
          foldingFunction (Just (x:y:ys)) "/" = if y `mod` x == 0 then Just ((y `div` x):ys) else Nothing
          foldingFunction (Just xs) numberString = Just (read numberString:xs)
          foldingFunction Nothing _ = Nothing

-- all RPN strings can be rearranged to "number number ... operator operator..."
-- We want something that will produce a list of all the possible RPN strings.

-- Countdown maths problem doesn't require all the numbers are used.  The minimum we can use is 2.
-- We need to also consider all the combinations of not all the tiles being used.
allRPNs :: [String] -> [String]
allRPNs = concat . fmap rpnPermutations . init . init . tails

rpnPermutations :: [String] -> [String]
rpnPermutations tiles = (++) <$> (tilePerms tiles) <*> (operatorPerms . length $ tiles)

tilePerms :: [String] -> [String]
tilePerms = addSpaces . fmap unwords . permutations

-- operatorPerms. Given a number we want to produce every combination of +,-,*,/ that is that number long minus 1.
-- As we know this number will be between 2 and 6, we can be lazy
operatorPerms :: Int -> [String]
operatorPerms x
  | x == 2 = ops
  | x == 3 = ops2
  | x == 4 = ops3
  | x == 5 = ops4
  | x == 6 = ops5
  where ops5 = (++) <$> ops4 <*> ops
        ops4 = (++) <$> ops3 <*> ops
        ops3 = (++) <$> ops2 <*> ops
        ops2 = (++) <$> ops <*> ops
        ops = addSpaces operators
        operators = ["+", "-", "*", "/"]

addSpaces :: [String] -> [String]
addSpaces = map (++" ")

-- From that list we want the string that produces the first successful calculation that has the value of our target.
getASolution :: [String] -> Int -> String
getASolution tiles target = head . getAllSolutions tiles $ target

getAllSolutions :: [String] -> Int -> [String]
getAllSolutions tiles target = [rpn | rpn <- allRPNs tiles, countdownRPN rpn == Just target]

