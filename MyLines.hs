module MyLines
( splitOnElem
, lines'
, unlines'
 ) where

import Data.List
import MyLists

-- Aside: my guess at how lines and unlines might be implemented
lines' = splitOnElem '\n'
unlines' = intercalate "\n"

