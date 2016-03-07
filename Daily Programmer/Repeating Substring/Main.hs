module Main where

import Data.List (find)
import Data.Maybe (fromMaybe)
import Data.List.Split (chunksOf)

findSequence :: String -> Maybe String
findSequence input = head <$> find cyclic partitions 
  where partitions = map (flip chunksOf input) [1.. length input `div` 2]
        cyclic s = and $ zipWith (==) s (tail s)

main = putStrLn . fromMaybe "There is no repeating substring." . findSequence =<< getLine