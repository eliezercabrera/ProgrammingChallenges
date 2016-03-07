module Main where

import Data.List
import Data.Ord
import Control.Applicative

type Coordinates = (Int, Int)
type Distance = Double
type Distance' = Int
type Capacity = Int
type Demand = Int

data Location = Location Coordinates Demand deriving (Eq, Show)

getDemand :: Location -> Demand
getDemand (Location _ d) = d

data Truck = Truck Coordinates Capacity Distance

getDistance :: Truck -> Distance
getDistance (Truck _ _ d) = d

data Truck' = Truck' Coordinates Capacity Distance'

getDistance' :: Truck' -> Distance'
getDistance' (Truck' _ _ d) = d

euclideanDistance :: Coordinates -> Coordinates -> Distance
euclideanDistance (x1, y1) (x2, y2) = sqrt $ fromIntegral (x1-x2)^(2 :: Int) + fromIntegral (y1-y2)^(2 :: Int)

distanceApproximation :: Coordinates -> Coordinates -> Int
distanceApproximation (x1, y1) (x2, y2) = (x1-x2)^(2 :: Int) + (y1-y2)^(2 :: Int)

tourDistanceApproximation :: Coordinates -> Capacity -> [Location] -> Int
tourDistanceApproximation depot maxCapacity = getDistance' . foldl travelLocations (Truck' depot maxCapacity 0)
    where travelLocations (Truck' origin capacity distance) (Location destination demand)
            | capacity >= demand = Truck' destination (capacity - demand) (distance + distanceApproximation origin destination)
            | otherwise = Truck' destination (maxCapacity - demand) (distance + distanceApproximation origin depot + distanceApproximation depot destination)

tourDistance :: Coordinates -> Capacity -> [Location] -> Distance
tourDistance depot maxCapacity = getDistance . foldl travelLocations (Truck depot maxCapacity 0.0)
    where travelLocations (Truck origin capacity distance) (Location destination demand)
            | capacity >= demand = Truck destination (capacity - demand) (distance + euclideanDistance origin destination)
            | otherwise = Truck destination (maxCapacity - demand) (distance + euclideanDistance origin depot + euclideanDistance depot destination)
                                
solve :: Int -> Capacity -> Coordinates -> [Location] -> Distance
solve n maxCapacity depot ls = let tours = permutations ls -- $ concatMap ((++ ls) . ($ repeat (Location depot (-maxCapacity))) . take) [0.. length ls - 2]
                               in tourDistance depot maxCapacity $ minimumBy (comparing $ tourDistanceApproximation depot maxCapacity) tours

readFirstLine :: String -> (Capacity, Coordinates)
readFirstLine s = let [capacity,depotCoordinates] = words s
                  in (read capacity, read depotCoordinates)

readLocations :: String -> Location
readLocations s = let [demand, locationCoordinates] = words s
                 in Location (read locationCoordinates) (read demand)

main :: IO ()
main = do
  depotLocation <- getLine
  let (maxCapacity, depotCoordinates) = readFirstLine depotLocation
  secondLine <- getLine
  let numberOfLocations = read secondLine
  otherLines <- getContents
  let locations = map (readLocations) $ lines otherLines
  print $ solve numberOfLocations maxCapacity depotCoordinates locations