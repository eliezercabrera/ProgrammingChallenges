module Main where

import Control.Monad
import Data.List
import Data.Ord

type Distance = Double
type Coordinates = (Double, Double)

euclideanDistance :: Coordinates -> Coordinates -> Distance
euclideanDistance (x1, y1) (x2, y2) = sqrt $ (x1-x2)^(2 :: Int) + (y1-y2)^(2 :: Int)

euclideanDistanceApprox :: Coordinates -> Coordinates -> Distance
euclideanDistanceApprox (x1, y1) (x2, y2) = (x1 - x2)^(2 :: Int) + (y1 - y2)^(2 :: Int)



closestPair :: [Coordinates] -> (Coordinates, Coordinates)
closestPair = go . sortBy (comparing fst)
    where go (c:cs) = minimum $ map (euclideanDistanceApprox c) cs

readCoordinates :: String -> Coordinates
readCoordinates = listToTuple . map read . words
    where listToTuple [c1, c2] = (c1, c2)

main :: IO ()
main = interact $ show . closestPair . map readCoordinates . lines