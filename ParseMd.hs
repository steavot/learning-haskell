module ParseMd
( FormattedChar(..)
, FLine
, parseMd
 ) where

import Data.List
import MyLines

-- Reading a line of Markdown:
--   Code encapsulated by `` take precidence.  Anything inside `` can't
--   be formatted as bold, italic, strikethrough.
--   Bold, italics & striktrhough can be mixed.
--   A character can be either Code, some mix of Bold, Italic or Strikethrough
--   or just plain text.
--
-- OK for now let's assume that a character must be one of the five.
-- And those five format types have an order of precedance from left to right
-- in the typeclass below.
data FormattedChar = Code Char | Strike Char | Bold Char | Italic Char | Plain Char deriving(Eq, Show)
type FLine = [FormattedChar]

-- Starting with ``:
--   There's got to be an even number of ` in a line. [Actually... leave this for now]
--   Once we split on `, odd number elements in the list are code.
--   Then whatever's left, we split up on ~ and map Strike to the lucky characters.
--   And so on...
parseMd :: String -> FLine
parseMd lyne =
    let a = zip [1..] . splitOnElem '`' $ lyne
        sbip = map Plain
        csbip = (\(x,y)->if x `mod` 2 == 0 then map Code y else sbip y)
    in foldl (++) [] . map csbip $ a

-- mdText :: FLine -> String


