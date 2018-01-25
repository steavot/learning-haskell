module ParseMd
( FormattedChar(..)
, FLine
, parseMd
 ) where

import Data.List
import MyLines

-- Let's assume that a character must have a format of either code, strikethrough, bold, italic or plaintext.
-- And those five formats have an order of precedence from left to right in the type below.
data FormattedChar = Code Char | Strike Char | Bold Char | Italic Char | Plain Char deriving(Eq, Show)
type FLine = [FormattedChar]

parseMd :: String -> FLine
parseMd lyne =
    let c' = '`'
        s' = '~'
        b' = '*'
        i' = '_'
        -- enumSplit produces and enumerated list of strings created from seperating
        -- a string on the special characters above.
        enumSplit :: (Eq t) => t -> [t] -> [(Int, [t])]
        enumSplit el lst = zip [1..] . splitOnElem el $ lst

        formatFor rule specialChar str = foldl (++) [] . map rule . enumSplit specialChar $ str

        -- Reading from the bottom we want to either format a block of characters with the
        -- type FormattedChar or pass it along the order of precedence to be formatted as
        -- something else.
        ip    = (\(x,y)->if x `mod` 2 == 0 then map Italic y else map Plain y)
        bip   = (\(x,y)->if x `mod` 2 == 0 then map Bold y   else formatFor ip i' y)
        sbip  = (\(x,y)->if x `mod` 2 == 0 then map Strike y else formatFor bip b' y)
        csbip = (\(x,y)->if x `mod` 2 == 0 then map Code y   else formatFor sbip s' y)

    in formatFor csbip c' lyne

-- mdText :: FLine -> String

