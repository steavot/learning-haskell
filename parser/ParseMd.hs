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

-- This needs to fold over an FLine and put in the special characters where needed and
-- output the line of text.
mdText :: FLine -> String
mdText fline =
    -- addCharscter will fold over the FLine and build up the line of text while recording
    -- the most recent type constructor encountered.
    let makeText :: FLine -> (FormattedChar, String)
	addChars :: (FormattedChar, String) -> FormattedChar-> (FormattedChar, String)
        whichChars :: FormattedChar -> FormattedChar -> String
	theCharOf :: FormattedChar -> Char

	theCharOf (Code y) = y
        theCharOf (Strike y) = y
        theCharOf (Bold y) = y
        theCharOf (Italic y) = y
        theCharOf (Plain y) = y

	whichChars (Plain a) (Plain b) = (theCharOf x):[]
	whichChars (Plain a) (Code b) = (theCharOf x):'`'
	whichChars (Plain a) (Stike b) = (theCharOf x):'~'
	whichChars (Plain a) (Bold b) = (theCharOf x):'*'
	whichChars (Plain a) (Italic b) = (theCharOf x):'_'

	whichChars (Code a) (Code b) = (theCharOf x):[]
	whichChars (Code a) (Plain b) = (theCharOf x):'`'
	whichChars (Code a) (Stike b) = (theCharOf x):'~':'`'
	whichChars (Code a)= (theCharOf x):'*':'`'
	whichChars (Code a)= (theCharOf x):'_':'`'

	whichChars (Strike a) (Strike b) = (theCharOf x):[]
	whichChars (Strike a) (Plain b) = (theCharOf x):'~'
	whichChars (Strike a) (Code b) = (theCharOf x):'`':'~'
	whichChars (Strike a)= (theCharOf x):'*':'~'
	whichChars (Strike a)= (theCharOf x):'_':'~'

	whichChars (Bold a) (Bold b) = (theCharOf x):[]
	whichChars (Bold a) (Plain b) = (theCharOf x):'*'
	whichChars (Bold a) (Code b) = (theCharOf x):'`':'*'
	whichChars (Bold a) (Stike b) = (theCharOf x):'~':'*'
	whichChars (Bold a) (Italic b) = (theCharOf x):'_':'*'

	whichChars (Italic a) (Italic b) = (theCharOf x):[]
	whichChars (Italic a) (Plain b) = (theCharOf x):'_'
	whichChars (Italic a) (Code b) = (theCharOf x):'`':'_'
	whichChars (Italic a) (Stike b) = (theCharOf x):'~':'_'
	whichChars (Italic a) (Italic b) = (theCharOf x):'*':'_'

	addChars (lastx, lyne) x = (x, (whichChars lastx x) ++ lyne)

        accumulator = (Plain '', "")
	makeText = foldr addChars accumulator

  in snd . makeText $ fline

