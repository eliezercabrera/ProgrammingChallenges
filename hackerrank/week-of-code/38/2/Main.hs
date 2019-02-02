module Main where

import Data.Map.Strict as Map

solve :: Integer -> [Integer] -> Int
solve difference list
    = (length list -)
    . Map.foldr max (minBound :: Int)
    . Map.fromListWith (+)
    . zipWith (\index elem -> (elem - index*difference, 1)) [0..]
    $ list

main :: IO ()
main = do
    [_, k] <- fmap read . words <$> getLine
    list <- fmap read . words <$> getLine
    print (solve k list)
