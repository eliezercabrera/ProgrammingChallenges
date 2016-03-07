module Main where

import Data.List (nub, intersect)
import Data.Bool (bool)

prime :: Int -> Int
prime n = prime' n 2

prime' :: Int -> Int -> Int
prime'  n d
  | n `mod` d == 0 = d
  | otherwise = prime' n (d+1)

primes' :: Int -> [Int]
primes' 1 = []
primes' n = [prime n] ++ (primes' (n `quot` (prime n)) )

ruthaaron :: Int -> Int -> Bool
ruthaaron x y = ((primes' x) `intersect` (primes' y) == []) && (sum (nub (primes' x)) == sum (nub (primes' y)))

isPrime :: Integral int => int -> Bool
isPrime = null . tail . primeFactors

primes :: Integral int => [int]
primes = 2 : filter isPrime [3, 5..]

primeFactors :: Integral int => int -> [int]
primeFactors = pF primes
    where pF (p:ps) n | n < p*p = [n]
                      | n `mod` p == 0 = p : pF (p:ps) (n `div` p)
                      | otherwise = pF ps n

isRuthAaronPair :: Integral int => int -> int -> Bool
isRuthAaronPair n1 n2
    = (n1 == n2 + 1 || n1 + 1 == n2)
    && sum (uniqueFactors n1) == sum (uniqueFactors n2)
    where uniqueFactors = nub . primeFactors

main :: IO () 
main = interact $ unlines . map test . tail . lines
    where test pair =  pair
                    ++ bool "INVALID" "VALID" (uncurry isRuthAaronPair (read pair))