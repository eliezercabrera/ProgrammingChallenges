module Main where

import Data.Time
import Data.List (nub)
import Data.Char (digitToInt)

isPrime :: Int -> Bool
isPrime = null . tail . primeFactors

primes :: [Int]
primes = 2 : filter isPrime [3, 5..]

primeFactors :: Int -> [Int]
primeFactors = go primes
    where go (p:ps) n
              | p*p > n   = [n]
              | r == 0    = p : go (p:ps) q
              | otherwise = go ps n
              where (q, r) = n `quotRem` p

primeReplacements :: Int -> Int
primeReplacements n
    = foldl max 0 
    [ length (filter isPrime $ map (foldl addDigit 0) replacements)
    | positions <- nub (init number)
    , let replacements = filter ((/= 0) . head) (replace positions number)
    , let addDigit num d = 10*num + d]
    where (number, size) = (map digitToInt (show n), length number)
          replace ds ns
            = do d <- [0 .. 9]
                 return (map (\x -> if x == ds then d else x) ns)
              
measure :: Show f => f -> IO NominalDiffTime
measure f = do
    start <- getCurrentTime
    print f
    end   <- getCurrentTime
    return (diffUTCTime end start)
                      
main :: IO ()
main = do
    print =<< measure ((head $ dropWhile ((< 8) . primeReplacements) primes))