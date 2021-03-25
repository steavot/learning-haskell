#!/usr/bin/env stack
-- stack --resolver lts-15.4 script --package split --package containers --package random
{-# LANGUAGE ViewPatterns #-}
import qualified Data.Sequence as S
import           Data.List.Split
import           System.Random

main :: IO ()
main = do
  contents <- readFile "sample-data.txt"
  let numbers =  S.fromList . fmap read . splitOn "," $ contents :: S.Seq Int

  qs numbers >>= print


qs :: Ord a =>  S.Seq a -> IO (S.Seq a)
qs (S.viewl -> S.EmptyL) = return S.empty
qs seq = do
  pivotAt <- getStdRandom (randomR (0, S.length seq - 1))
  let pivot = S.index seq pivotAt
  let (smaller, bigger) = S.partition (<=pivot) (S.deleteAt pivotAt seq)
  below <- qs smaller
  above <- qs bigger
  return $ (<>) below $ pivot S.<| above
