-- Reading through the chapters of learn yourself a haskell..
-- Using the example of prime numbers to exercise concepts.
-- As a better way to do something appears, I will prime the previously
-- working function (or comment out if it's only a section of a function.)
module MyPrimes
( isPrime
, primesAbove
, firstNPrimes
, nthPrime
, nextPrimeAbove
 ) where

import Data.List

-- function to tell if all elements of a boolean array are false
allFalse = all (==False)

-- function to do sqrt for Ints
sqrtInt :: Int -> Int
sqrtInt = \n -> last [x | x <- [1..n], x*x <= n]
sqrtInt' n = last [x | x <- [1..n], x*x <= n]

-- function for telling if a number is prime (older versions in comment)
isPrime :: Int -> Bool
isPrime n
    | n < 2 = False
    | n < 4  = True
    | otherwise = allFalse . map (\x -> (n `mod` x) == 0) $ [2..sqrtInt n]
    -- | otherwise = allFalse . map (==0) . map (n `mod`) $ [2..sqrtInt' n]
    -- | otherwise = allFalse $ map (==0) $ map (n `mod`) [2..sqrtInt' n]
    -- | otherwise = allFalse' (map (==0) (map (n `mod`) [2..sqrtInt' n]))

-- infinite list of all primes above a number
primesAbove = \n -> [x | x <- [n..], isPrime x]

-- first n primes
firstNPrimes :: Int -> [Int]
firstNPrimes = (flip take)(primesAbove 2)
firstNPrimes' n = take n [x | x <- [2..],  isPrime x]

-- nth prime
nthPrime :: Int -> Int
nthPrime = last . firstNPrimes
nthPrime' = \n -> last $ firstNPrimes n
nthPrime'' n = last $ firstNPrimes n

-- first prime above a number
nextPrimeAbove :: Int -> Int
nextPrimeAbove = head . primesAbove
