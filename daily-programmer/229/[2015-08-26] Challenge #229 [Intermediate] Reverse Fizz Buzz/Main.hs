module Main where

import Data.List ((!!), isPrefixOf)
import Control.Monad (replicateM)

candidates :: [String] -> [Char]
candidates sequences = let maxChar = maximum $ map maximum sequences
                           minChar = minimum $ map minimum sequences
                       in  [minChar .. maxChar]

fizzbuzz :: [Int] -> [String]
fizzbuzz ns = let letters = zip ns ['a'..]
                  toFB number = concatMap (condition number) letters
                  condition n (n', c) | n `mod` n' == 0 = [c]
                                      | otherwise = []
              in filter (not . null) $ map toFB [1..]

-- Verifies relative ordering of each pairs of elements in both lists
similar :: [Int] -> [Int] -> Bool
similar xs ys = let indices = [0 .. length xs - 1]
                    pairs = replicateM 2 indices
                in all (\[i, j] -> compare (xs !! i) (xs !! j)
                                == compare (ys !! i) (ys !! j)) pairs

generate :: [Int] -> [[Int]]
generate relations = gen (length relations)
    where gen 0 = []
                                
getFizzbuzz :: [String] -> [[Int]]
getFizzbuzz input = head . filter (not . null) $ map next [1..]
    where indices = map (\c -> length $ takeWhile (c `notElem`) input) (candidates input)
          next n = [ c | c <- replicateM (length indices) [n .. n + 30]
                       , similar c indices
                       , input `isPrefixOf` fizzbuzz c]
              
main = interact $ unwords . map show . minimum . getFizzbuzz . lines