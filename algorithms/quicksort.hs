#!/usr/bin/env stack
-- stack --resolver lts-15.4 script
import           Data.List
import           Data.List.Split

main :: IO ()
main = do
  contents <- readFile "sample-data.txt"
  let numbers =  fmap read . splitOn "," $ contents :: [Int]

  print $ qs numbers


qs :: Ord a =>  [a] -> [a]
qs [] = []
qs (x:[]) = [x]
qs (x:xs) = qs smaller ++ [x] ++ qs bigger
   where
     (smaller, bigger) = partition (<=x) xs
