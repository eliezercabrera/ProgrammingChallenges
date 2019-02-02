module Main where

import Data.Char (digitToInt)
import Data.Numbers.Primes (primes)

happy :: Int -> Bool
happy = (`elem` [1..10]) . length . filter (== 1) . take 10000 . iterate (sum . map ((^2) . digitToInt) . show)

main = putStr . unlines . map show . filter happy . takeWhile (<= 10000) $ primes