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
-- And those five format types have an order of precedence from left to right
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
    let c' = '`'
        s' = '~'
        b' = '*'
        i' = '_'
        -- a, produces and enumerated list of strings created from seperating
        -- a string on the formatting characters above.
        a :: Char -> String -> [(Int, String)]
        a c l = zip [1..] . splitOnElem c $ l

        -- reading from the bottom we want to either format the character as a
        -- type or pass it along the order of precedence.
        ip    = (\(x,y)->if x `mod` 2 == 0 then map Italic y else map Plain y)
        bip   = (\(x,y)->if x `mod` 2 == 0 then map Bold y   else foldl (++) [] . map ip . a i' $ y)
        sbip  = (\(x,y)->if x `mod` 2 == 0 then map Strike y else foldl (++) [] . map bip . a b' $ y)
        csbip = (\(x,y)->if x `mod` 2 == 0 then map Code y   else foldl (++) [] . map sbip . a s' $ y)
    in foldl (++) [] . map csbip . a c' $ lyne

-- mdText :: FLine -> String


