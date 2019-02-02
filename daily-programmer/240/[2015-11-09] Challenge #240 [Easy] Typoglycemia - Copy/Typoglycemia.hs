module Typoglycemia where

import Data.Ord        (comparing)
import Data.List       (sortBy)
import Data.Char       (isAlpha)
import Data.List.Split (chunksOf)
import System.Random   (randomRs, newStdGen)


shuffle xs = fst . unzip . sortBy (comparing snd) . zip xs

trueWords :: String -> [String]
trueWords [] = []
trueWords (c:cs) = current : trueWords rest
    where (current, rest) = span (f . isAlpha) (c:cs)
          f | isAlpha c = id
            | otherwise = not

typoglyfy :: [[Int]] -> String -> [String]
typoglyfy ns = zipWith scramble ns . trueWords
    where scramble rs (c:cs)
              | isAlpha c = c : shuffle (init cs) rs ++ [last cs]
              | otherwise = c:cs

main = do
    rs <- chunksOf 20 . randomRs (1, 20) <$> newStdGen
    interact $ concat . typoglyfy rs