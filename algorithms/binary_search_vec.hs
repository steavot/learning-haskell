#!/usr/bin/env stack
-- stack --resolver lts-15.4 script --package vector --package split
{-# LANGUAGE OverloadedStrings #-}
import qualified Data.Vector as V
import Data.List.Split

main :: IO ()
main = do
  contents <- readFile "ordered-data.txt"
  let numbers =  V.fromList . fmap read . splitOn "," $ contents :: V.Vector Int
  print "give number to find"
  lookingFor <- fmap read getLine :: IO Int

  print $ bs lookingFor numbers


bs lf xs
  | length xs == 0 = False
  | length xs == 1 = lf == V.head xs
  | otherwise = lf == halfWay || (lf < halfWay && bs lf below) || bs lf above
   where
     (below, notBelow) = V.splitAt (V.length xs `div` 2) xs
     halfWay = V.head notBelow
     above = V.tail notBelow
