module MyLines
( splitOnElem
, lines'
, unlines'
 ) where

import Data.List

-- Aside: my guess at how lines and unlines might be implemented
splitOnElem :: (Eq t) => t -> [t] -> [[t]]
splitOnElem x text
    | x `notElem` text = [text]
    | x `elem` text =
        let pre = takeWhile (/=x) text
            post = tail $ dropWhile (/=x) text
        in [pre] ++ (splitOnElem x $ post)

lines' = splitOnElem '\n'
unlines' = intercalate "\n"

