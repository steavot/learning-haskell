import Data.List
import MyLines
import ParseMd

-- data FormattedLine = Header1 String | Header2 String | Text FLine deriving(Eq, Show)
data FormattedLine = Header1 String | Header2 String | Text [Char] deriving(Eq, Show)

-- function to take in a string for a line of markdown and return a constructor of FormattedLine
readMd :: String -> FormattedLine
readMd lyne
    | "# " `isPrefixOf` lyne = Header1 (let (_, text) = splitAt 2 lyne in text)
    | "## " `isPrefixOf` lyne = Header2 (let (_, text) = splitAt 3 lyne in text)
--    | otherwise = Text . parseMd lyne
    | otherwise = Text lyne

-- produce a string in the format desired ... matching on type constructor
writeMd :: FormattedLine -> String
writeHTML :: FormattedLine -> String

writeMd (Header1 x) = "# " ++ x
writeMd (Header2 x) = "## " ++ x
writeMd (Text x) = x

writeHTML (Header1 x) = "<h1>" ++ x ++ "</h1>"
writeHTML (Header2 x) = "<h2>" ++ x ++ "</h2>"
writeHTML (Text x) = "<b>" ++ x ++ "</b>"

-- functions to deal with entire blocks of formatted text
readMdBlock :: String -> [FormattedLine]
writeMdBlock :: [FormattedLine] -> String
writeHTMLBlock :: [FormattedLine] -> String

readMdBlock block = map readMd $ lines' $ block

writeMdBlock stuff = unlines' . map writeMd $ stuff
writeHTMLBlock stuff = unlines' . map writeHTML $ stuff

