-- function to tell if all elements of a boolean array are false
allFalse = not . foldl1 (||)

-- function to do sqrt for Ints
sqrtInt' :: Int -> Int
sqrtInt' n = last [x | x <- [1..n], x*x <= n]

-- function for telling if a number is prime (older versions in comment)
isPrime' :: Int -> Bool
isPrime' n
    | n < 2 = False
    | n < 4  = True
    | otherwise = allFalse . map (\x -> (n `mod` x) == 0) $ [2..sqrtInt' n]
    -- | otherwise = allFalse . map (==0) . map (n `mod`) $ [2..sqrtInt' n]
    -- | otherwise = allFalse $ map (==0) $ map (n `mod`) [2..sqrtInt' n]
    -- | otherwise = allFalse' (map (==0) (map (n `mod`) [2..sqrtInt' n]))

-- infinite list of all primes above a number
primesAbove = \n -> [x | x <- [n..], isPrime' x]

-- first n primes
firstNPrimes :: Int -> [Int]
firstNPrimes = (flip take)(primesAbove 2)
-- firstNPrimes n = take n [x | x <- [2..],  isPrime' x]

-- nth prime
nthPrime :: Int -> Int
nthPrime n = last $ firstNPrimes n
nthPrime' = \n -> last $ firstNPrimes n
nthPrime'' = last . firstNPrimes

-- first prime above a number
nextPrimeAbove :: Int -> Int
nextPrimeAbove = head . primesAbove
