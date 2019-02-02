module Main where

import Data.Function (on)
import Data.List (group, groupBy, sort)

import qualified Data.Array as Array
import qualified Data.Map.Strict as Map

allMatchOn :: (Eq b) => (a -> b) -> [a] -> Bool
-- This implementation works too.
-- allMatchOn f = null . drop 1 . groupBy ((==) `on` f)
allMatchOn f = and . (zipWith ((==) `on` f) <*> tail)

bonus :: String -> Bool
bonus = allMatchOn length . group . sort

bonusArray :: String -> Bool
bonusArray =
  allMatchOn id .
  filter ((/= 0)) .
  Array.elems . Array.accumArray (+) 0 ('a', 'z') . flip zip (repeat 1)

bonusMap :: String -> Bool
bonusMap =
  allMatchOn id . Map.elems . foldr (flip (Map.insertWith (+)) 1) Map.empty

main :: IO ()
main =
  interact $
  unlines .
  fmap (show . (\s -> allMatchOn ($ s) [bonus, bonusArray, bonusMap])) . lines
