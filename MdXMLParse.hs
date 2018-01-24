-- module format: small to big, but start in the middle... if you want.
import Data.List
import MyLines

data FormattedChar = Bold Char | Italic Char | Strike Char | Plain Char deriving(Eq, Show)
type FLine = [FormattedChar]

-- data Thing = Header1 String | Header2 String | Text FLine deriving(Eq, Show)
data Thing = Header1 String | Header2 String | Text [Char] deriving(Eq, Show)

-- function to take in a string for a line of markdown and return a constructor of Thing
reader :: String -> Thing
reader lyne
    | "# " `isPrefixOf` lyne = Header1 (let (_, text) = splitAt 2 lyne in text)
    | "## " `isPrefixOf` lyne = Header2 (let (_, text) = splitAt 3 lyne in text)
--    | otherwise = Text . parseFLine line
    | otherwise = Text lyne

-- produce a string in the format desired ... matching on type constructor
writeMd :: Thing -> String
writeHTML :: Thing -> String

writeMd (Header1 x) = "# " ++ x
writeMd (Header2 x) = "## " ++ x
writeMd (Text x) = x

writeHTML (Header1 x) = "<h1>" ++ x ++ "</h1>"
writeHTML (Header2 x) = "<h2>" ++ x ++ "</h2>"
writeHTML (Text x) = x

-- functions to deal with entire blocks of formatted text
readMdBlock :: String -> [Thing]
writeMdBlock :: [Thing] -> String
writeHTMLBlock :: [Thing] -> String

readMdBlock block = map reader $ lines' $ block

writeMdBlock stuff = unlines' . map writeMd $ stuff
writeHTMLBlock stuff = unlines' . map writeHTML $ stuff

