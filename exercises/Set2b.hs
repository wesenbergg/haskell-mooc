module Set2b where

import Mooc.Todo


import Data.List


binomial :: Integer -> Integer -> Integer
binomial _ 0 = 1
binomial 0 k = 0
binomial n k = binomial (n-1) k + binomial (n-1) (k-1)

oddFactorial :: Integer -> Integer
oddFactorial n
  | n <= 0    = 1
  | odd n     = n * oddFactorial (n-2)
  | otherwise = oddFactorial (n-1)

myGcd :: Integer -> Integer -> Integer
myGcd a 0 = a
myGcd 0 b = b
myGcd a b
  | a > b     = myGcd (a - b) b
  | otherwise = myGcd a (b - a)

leftpad :: String -> Int -> String
leftpad str n
  | length str >= n = str
  | otherwise       = leftpad (" " ++ str) n

countdown :: Integer -> String
countdown n = "Ready! " ++ countHelper n ++ "Liftoff!"
  where
    countHelper 0 = " "
    countHelper x = show x ++ "... " ++ countHelper (x - 1)

smallestDivisor :: Integer -> Integer
smallestDivisor n = findDivisor n 2  
  where
    findDivisor num d
      | num `mod` d == 0 = d
      | otherwise        = findDivisor num (d + 1)

isPrime :: Integer -> Bool
isPrime n
  | n < 2     = False
  | otherwise = smallestDivisor n == n

biggestPrimeAtMost :: Integer -> Integer
biggestPrimeAtMost n
  | isPrime n = n
  | otherwise = biggestPrimeAtMost (n - 1)
