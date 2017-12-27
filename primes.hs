-- function to tell if all elements of a boolean array are false
anyTrue' :: [Bool] -> Bool
anyTrue' [] = error "empty list"
anyTrue' (a:[]) = a
anyTrue' x = head x || anyTrue' (tail x)

allFalse' x = not (anyTrue' x)

-- function to do sqrt for Ints
sqrtInt' :: Int -> Int
sqrtInt' n = maximum [x | x <- [1..n], x*x <= n]

-- function for telling if a number is prime
isPrime' :: Int -> Bool
isPrime' n
    | n < 2 = False
    | n < 4  = True
    | otherwise = allFalse' (map (==0) (map (n `mod`) [2..sqrtInt' n]))

-- first n primes
firstNPrimes :: Int -> [Int]
firstNPrimes n = take n [x | x <- [2..],  isPrime' x]

-- nth prime
nthPrime :: Int -> Int
nthPrime n = last (firstNPrimes n)

-- first prime above a number
nextPrimeAbove :: Int -> Int
nextPrimeAbove n = head [x | x <- [n..], isPrime' x]