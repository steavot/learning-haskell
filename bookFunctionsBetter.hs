-- Going through the "Learn You a Haskell" book, if I see something I think
-- could look nicer, re-do it here:
import Data.List

-- From Modules chapter, using fold to implement isInfixOf.  The book writes:
search :: (Eq a) => [a] -> [a] -> Bool
search needle haystack =
    let nlen = length needle
    in  foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- My version
search' needle haystack =
    let nlen = length needle
    in or . map (\x -> take nlen x == needle) $ (tails haystack)