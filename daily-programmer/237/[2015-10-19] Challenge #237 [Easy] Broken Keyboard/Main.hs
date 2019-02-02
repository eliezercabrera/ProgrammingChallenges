module Main where

import Data.List
import Data.Ord
import Data.Set (fromList, isSubsetOf, toList)

main = do
    dictionaryWords  <- map fromList . lines <$> readFile "enable1.txt"
    interact $ unlines . map (input dictionaryWords) . tail . lines
    where input dictionaryWords letters
              = let valid = (`isSubsetOf` fromList letters)
                    possible = filter valid dictionaryWords
                 in letters ++ if null possible
                                   then ": no possible words"
                                   else let best = maximumBy (comparing length) possible
                                         in " = " ++ toList best