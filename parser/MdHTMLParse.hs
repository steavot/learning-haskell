module MdHTMLParse
( readMdBlock
, writeMdBlock
, writeHTMLBlock
, writeMd
, writeHTML
) where

import Data.List
import MyLines
import ParseMd

data FormattedLine = Header1 String | Header2 String | Text FLine deriving(Eq, Show)

-- function to take in a string for a line of markdown and return a constructor of FormattedLine
readMd :: String -> FormattedLine
readMd lyne
  | "# " `isPrefixOf` lyne = Header1 (let (_, text) = splitAt 2 lyne in text)
  | "## " `isPrefixOf` lyne = Header2 (let (_, text) = splitAt 3 lyne in text)
  | otherwise = Text . parseMd $ lyne

-- produce a string in the format desired ... matching on type constructor
writeMd :: FormattedLine -> String
writeMd (Header1 x) = "# " ++ x ++ "\n"
writeMd (Header2 x) = "## " ++ x ++ "\n"
writeMd (Text x) = mdText x ++ "\n"

writeHTML :: FormattedLine -> String
writeHTML (Header1 x) = "<h1>" ++ x ++ "</h1>"
writeHTML (Header2 x) = "<h2>" ++ x ++ "</h2>"
writeHTML (Text x) = "<br>" ++ htmlText x ++ "</br>"

-- functions to deal with entire blocks of formatted text
readMdBlock :: String -> [FormattedLine]
readMdBlock = map readMd . lines'

writeMdBlock :: [FormattedLine] -> String
writeMdBlock = intercalate [] . map writeMd

writeHTMLBlock :: [FormattedLine] -> String
writeHTMLBlock = intercalate [] . map writeHTML

