module Main where

import System.Environment
import Data.Time

import Control.Monad
import qualified Data.Numbers.Primes as P
import qualified Data.Map as Map
import qualified Data.PQueue.Prio.Min as PQ

isPrime :: Integral int => int -> Bool
isPrime = null . tail . primeFactors

primes :: Integral int => [int]
primes = 2 : 3 : 5 : 7 : filter isPrime (spin wheel2357 11)

primeFactors :: Integral int => int -> [int]
primeFactors = pF primes
    where pF (p:ps) n | n < p*p = [n]
                      | n `mod` p == 0 = p : pF (p:ps) (n `div` p)
                      | otherwise = pF ps n

wheel2357 :: Integral int => [int]
wheel2357 = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel2357

spin :: Integral int => [int] -> int -> [int]
spin (x:xs) n = n : spin xs (n + x)

primesUnfaithfull :: Integral int => [int]
primesUnfaithfull = 2 : 3 : 5 : 7 : sieve (spin wheel2357 11)
    where sieve (p:xs) = p : sieve [x | x <- xs, x `mod` p > 0]

primesTrialDivision :: Integral int => [int]
primesTrialDivision = 2 : 3 : 5 : 7 : filter isPrime'' (spin wheel2357 11)
    where isPrime'' x = all ((> 0) . (x `mod`)) (factorsToTry x)
          factorsToTry x = takeWhile (\p -> p^2 <= x) primes

primesQueue :: Integral int => [int]
primesQueue = 2 : 3 : 5 : 7 : sieve (spin wheel2357 11)
    where sieve [] = []
          sieve (x:xs) = x : sieve' xs (insertPrime x xs PQ.empty)
            where insertPrime p xs table = PQ.insert (p^2) (map (*p) xs) table
                  sieve' [] _ = []
                  sieve' (x:xs) table | nextComposite <= x = sieve' xs (adjust table)
                                      | otherwise = x : sieve' xs (insertPrime x xs table)
                                        where nextComposite = fst $ PQ.findMin table
                                              adjust table | n <= x = adjust (PQ.insert n' ns $ PQ.deleteMin table)
                                                           | otherwise = table
                                                             where (n, n':ns) = PQ.findMin table

timeFunction :: Int -> [Int] -> IO (NominalDiffTime)
timeFunction n f = do
  start <- getCurrentTime
  print . last $ take n f
  end   <- getCurrentTime
  return $ diffUTCTime end start

main :: IO ()
main = do
    n <- read . head <$> getArgs
    let fs = [primesTrialDivision, primesQueue, primes, P.primes] :: [[Int]]
    times <- mapM (timeFunction n) fs
    putStrLn . unlines . map show $ times