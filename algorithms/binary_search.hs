#!/usr/bin/env stack
-- stack --resolver lts-15.4 script --package split
import           Data.List
import           Data.List.Split

main :: IO ()
main = do
  contents <- readFile "ordered-data.txt"
  let numbers =  fmap read . splitOn "," $ contents :: [Int]
  print "give number to find"
  lookingFor <- fmap read getLine :: IO Int

  print $ bs lookingFor numbers


bs :: Ord a => Eq a => a -> [a] -> Bool
bs _ []  = False
bs lf (x:[]) = lf == x
bs lf xs = lf == halfWay || (lf < halfWay && bs lf below) || bs lf above
   where
     (below, halfWay:above) = splitAt (length xs `div` 2) xs
