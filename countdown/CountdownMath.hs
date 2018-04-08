-- Module of functions that say whether the target number can be made from the inputs and only operators +-*/
-- Inputs: up to 4 from [25, 50, 75, 100] and the rest from
-- [1, 1, 2 , 2 , 3 , 3 , 4 , 4 , 5 , 5 , 6 , 6 , 7 , 7 , 8 , 8 , 9 , 9 , 10 , 10] to make 6 numbers in total.
-- Target is a random integer between [101, 999]
-- Not all inputs need to be used
-- Running total cannot be -ve or a fraction
module CountdownMath
( countdownRPN
, operatorPerms
, combineOpsAndTiles
, rpnPermutations
, allRPNs
, getASolution
, getAllSolutions
) where

import Data.List
import GHC.Exts


-- RL You don't need ln and sum - you only use binary operations which makes RPNs a little
-- more predictable and easier encode (I know this is just a verbatim copy)
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


-- RL Do you want to represent an RPN as a string? Presumably this
--    makes it easier to generate expressions. I wonder if there are clever
--    ways of efficiently generating RPN patterns which are "valid". Here is
--    probably where I would start:
--
-- newtype RPN = RPN [String]  (or type RPN = [String] if you prefer)
--
-- isOp :: String -> Bool
-- decodeOp :: String -> (Float -> Float -> Maybe Float)
-- eval :: RPN -> Float
-- rank :: RPN -> Int    -- height of stack after evaluation - note you don't need
                         -- to know about the numbers at all, only that they are numbers
                         -- you want to filter low rank (<1) RPNs

-- For our purposes we want:
--   Just the 1st 4 operators
--   Evaluation to fail if the value on the top of the stack becomes either negative or a fraction
--   Evaluation to fail for invalid RPN strings
-- As our inputs for this are always from the lists above we do not need to worry about:
--   Addition resulting in a negative
--   Multiplication resulting in a negative
-- Therefore the calculations that can fail are:
--   Division by zero (this is covered by not allowing subtraction of equal numbers)
--   Division resulting in a fraction
--   Subtraction resulting in a negative
-- RL decodeOp above will let you decouple the operation logic from the aggregation
countdownRPN :: String -> Maybe Int
countdownRPN = fmap head . (foldl foldingFunction (Just [])) . words
    where foldingFunction :: Maybe [Int] -> String -> Maybe [Int]
          -- Accumulator is Nothing - the calculation has already failed
          -- RL See Control.Monad's foldM
          foldingFunction Nothing _ = Nothing
          -- Dealing with valid RPN strings from here
          foldingFunction (Just (x:y:ys)) "*" = Just ((x * y):ys)
          foldingFunction (Just (x:y:ys)) "+" = Just ((x + y):ys)
          foldingFunction (Just (x:y:ys)) "-" = if y > x then Just ((y - x):ys) else Nothing
          foldingFunction (Just (x:y:ys)) "/" = if y `mod` x == 0 then Just ((y `div` x):ys) else Nothing
          foldingFunction (Just xs) string
            | string `elem` ["+", "-", "*", "/"] = Nothing
            | otherwise = Just ((read string :: Int):xs)    -- RL make read use Maybe

-- Countdown maths problem doesn't require all the numbers are used.  The minimum we can use is 2.
-- We need to also consider all the combinations of not all the tiles being used.
allRPNs :: [String] -> [String]
allRPNs tiles = nub . concat . fmap rpnPermutations . tileCombines $ tiles
  where tileCombines tiles = sortWith length [t | t <- subsequences tiles, length t > 1]

-- RL You calculate operatorPerms and then essentially do it again in combineOpsAndTiles
-- Can that be avoided?
rpnPermutations :: [String] -> [String]
rpnPermutations tiles = concat (fmap (combineOpsAndTiles tiles) (operatorPerms . length $ tiles))

combineOpsAndTiles :: [String] -> String -> [String]
combineOpsAndTiles tiles ops = fmap unwords . permutations $ (tiles ++ (words ops))

-- RL operatorPerms = mapM (const operators) (replicate (x-1) 0)  note the 0 has no relevance here, we just need a list of length x-1 to map over
-- operatorPerms. Given a number we want to produce every combination of +,-,*,/ that is that number long minus 1. -- RL subtracting 1 here leads to a slightly misleading API
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

