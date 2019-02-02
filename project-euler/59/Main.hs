module Main where

import Data.Bits (xor)
import Data.Ord (comparing)
import Data.List (maximumBy)
import Data.List.Split (splitOn)
import Control.Monad (replicateM)

type EcryptedLetter = Int
    
decrypt :: [EcryptedLetter] -> Int
decrypt ls
    = sum
    $ maximumBy (comparing $ length . filter (== fromEnum ' '))
    [ zipWith xor ls (cycle key) | key <- keys ]
    where keys = map fromEnum <$> replicateM 3 ['a' .. 'z']

main :: IO ()
main = interact $ show . decrypt . map read . splitOn ","