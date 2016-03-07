module Main where

import Control.Monad
import Data.List
import Data.Ord
import System.Environment
import Data.Time

import qualified Data.Set as Set
import qualified QuadTree as Q

origin :: Q.Coordinate
origin = (0.5, 0.5)

processNextPoint :: Q.QuadTree -> Q.Coordinate -> (Q.QuadTree, Q.Coordinate, Q.Distance)
processNextPoint qt c = let closest  = Q.findClosest c qt
                            newQT    = Q.delete closest qt
                            distance = Q.euclideanDistance c closest
                        in (newQT, closest, distance)
                            
processPointList :: Q.QuadTree -> Q.Coordinate -> Q.Distance
processPointList ps c = let (newQT, closest, d) = processNextPoint ps c
                        in if Q.isEmpty newQT then d else d + processPointList newQT closest

processLine :: IO Q.Coordinate
processLine = do
    [x, y] <- return . fmap read . words =<< getLine
    return (x, y)

main :: IO ()
main = do
    numberOfLines <- return . read =<< getLine
    size <- return . read . head =<< getArgs
    start <- getCurrentTime
    coordinates <- return . foldl (flip Q.insert) (Q.QuadTree size (Q.empty origin 0.5)) =<< replicateM numberOfLines processLine
    let travelDistance = processPointList coordinates origin
    print travelDistance
    end   <- getCurrentTime
    print $ diffUTCTime end start