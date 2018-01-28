module ParseMd
( FormattedChar(..)
, FLine
, parseMd
, mdText
, htmlText
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

mdText :: FLine -> String
mdText fline =
    let whichChars :: FormattedChar -> FormattedChar -> String
        whichChars (Plain a) (Plain b) = b:[]
        whichChars (Plain a) (Code b) = b:'`':[]
        whichChars (Plain a) (Strike b) = b:'~':[]
        whichChars (Plain a) (Bold b) = b:'*':[]
        whichChars (Plain a) (Italic b) = b:'_':[]

        whichChars (Code a) (Code b) = b:[]
        whichChars (Code a) (Plain b) = b:'`':[]
        whichChars (Code a) (Strike b) = b:'~':'`':[]
        whichChars (Code a) (Bold b) = b:'*':'`':[]
        whichChars (Code a) (Italic b) = b:'_':'`':[]

        whichChars (Strike a) (Strike b) = b:[]
        whichChars (Strike a) (Plain b) = b:'~':[]
        whichChars (Strike a) (Code b) = b:'`':'~':[]
        whichChars (Strike a) (Bold b) = b:'*':'~':[]
        whichChars (Strike a) (Italic b) = b:'_':'~':[]

        whichChars (Bold a) (Bold b) = b:[]
        whichChars (Bold a) (Plain b) = b:'*':[]
        whichChars (Bold a) (Code b) = b:'`':'*':[]
        whichChars (Bold a) (Strike b) = b:'~':'*':[]
        whichChars (Bold a) (Italic b) = b:'_':'*':[]

        whichChars (Italic a) (Italic b) = b:[]
        whichChars (Italic a) (Plain b) = b:'_':[]
        whichChars (Italic a) (Code b) = b:'`':'_':[]
        whichChars (Italic a) (Strike b) = b:'~':'_':[]
        whichChars (Italic a) (Bold b) = b:'*':'_':[]

        addChars :: FormattedChar -> (FormattedChar, String) -> (FormattedChar, String)
        addChars x (lastx, lyne) = (x, (whichChars lastx x) ++ lyne)

        makeText :: FLine -> (FormattedChar, String)
        makeText = foldr addChars (Plain 'a', "")

  in snd . makeText $ fline

htmlText :: FLine -> String
htmlText fline =
    let whichChars :: FormattedChar -> FormattedChar -> String
        whichChars (Plain a) (Plain b) = b:[]
        whichChars (Plain a) (Code b) = b:'<':'/':'t':'t':'>':[]
        whichChars (Plain a) (Strike b) = b:'<':'/':'s':'t':'r':'i':'k':'e':'>':[]
        whichChars (Plain a) (Bold b) = b:'<':'/':'b':'>':[]
        whichChars (Plain a) (Italic b) = b:'<':'/':'i':'>':[]

        whichChars (Code a) (Code b) = b:[]
        whichChars (Code a) (Plain b) = b:'<':'t':'t':'>':[]
        whichChars (Code a) (Strike b) = b:'<':'/':'s':'t':'r':'i':'k':'e':'>':'<':'t':'t':'>':[]
        whichChars (Code a) (Bold b) = b:'<':'/':'b':'>':'<':'t':'t':'>':[]
        whichChars (Code a) (Italic b) = b:'<':'/':'i':'>':'<':'t':'t':'>':[]

        whichChars (Strike a) (Strike b) = b:[]
        whichChars (Strike a) (Plain b) = b:'<':'s':'t':'r':'i':'k':'e':'>':[]
        whichChars (Strike a) (Code b) = b:'<':'/':'t':'t':'>':'<':'s':'t':'r':'i':'k':'e':'>':[]
        whichChars (Strike a) (Bold b) = b:'<':'/':'b':'>':'<':'s':'t':'r':'i':'k':'e':'>':[]
        whichChars (Strike a) (Italic b) = b:'<':'/':'i':'>':'<':'s':'t':'r':'i':'k':'e':'>':[]

        whichChars (Bold a) (Bold b) = b:[]
        whichChars (Bold a) (Plain b) = b:'<':'b':'>':[]
        whichChars (Bold a) (Code b) = b:'<':'/':'t':'t':'>':'<':'b':'>':[]
        whichChars (Bold a) (Strike b) = b:'<':'/':'s':'t':'r':'i':'k':'e':'>':'`':'<':'b':'>':[]
        whichChars (Bold a) (Italic b) = b:'<':'/':'i':'>':'<':'b':'>':[]

        whichChars (Italic a) (Italic b) = b:[]
        whichChars (Italic a) (Plain b) = b:'<':'i':'>':[]
        whichChars (Italic a) (Code b) = b:'<':'/':'t':'t':'>':'<':'i':'>':[]
        whichChars (Italic a) (Strike b) = b:'<':'/':'s':'t':'r':'i':'k':'e':'>':'`':'<':'i':'>':[]
        whichChars (Italic a) (Bold b) = b:'<':'/':'b':'>':'<':'i':'>':[]

        addChars :: FormattedChar -> (FormattedChar, String) -> (FormattedChar, String)
        addChars x (lastx, lyne) = (x, (whichChars lastx x) ++ lyne)

        makeText :: FLine -> (FormattedChar, String)
        makeText = foldr addChars (Plain 'a', "")

  in snd . makeText $ fline

