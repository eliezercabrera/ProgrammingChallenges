module Main where

import Control.Monad
import Data.List
import Data.Ord

import qualified QuadTree as QuadTree

type Coord = (Double, Double)
origin :: Coord
origin = (0.5, 0.5)

euclideanDistance :: Coord -> Coord -> Double
euclideanDistance (x1, y1) (x2, y2) = sqrt $ (x1-x2)^(2 :: Int) + (y1-y2)^(2 :: Int)

processNextPoint :: [Coord] -> Coord -> ([Coord], Coord, Double)
processNextPoint ps c = let closest  = minimumBy (comparing $ euclideanDistance c) ps
                            newList  = delete closest ps
                            distance = euclideanDistance c closest
                        in (newList, closest, distance)
                            
processPointList :: [Coord] -> Coord -> Double
processPointList ps c = case processNextPoint ps c of
                        ([], _, d) -> d
                        (newList, closest, d) -> d + processPointList newList closest

processLine :: IO Coord
processLine = do
    [x, y] <- return . fmap read . words =<< getLine
    return (x, y)

main :: IO ()
main = do
    numberOfLines <- return . read =<< getLine
    listOfCoordinates <- replicateM numberOfLines processLine
    let travelDistance = processPointList listOfCoordinates origin
    print travelDistance