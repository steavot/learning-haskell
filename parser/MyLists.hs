module MyLists
( splitOnElem
, numberOfIn
 ) where

import Data.List

splitOnElem :: (Eq t) => t -> [t] -> [[t]]
splitOnElem x text
  | x `notElem` text = [text]
  | x `elem` text =
      let pre = takeWhile (/=x) text
          post = tail $ dropWhile (/=x) text
       in pre:(splitOnElem x $ post)

numberOfIn :: (Eq t) => t -> [t] -> Int
numberOfIn x l = length . elemIndices x $ l

