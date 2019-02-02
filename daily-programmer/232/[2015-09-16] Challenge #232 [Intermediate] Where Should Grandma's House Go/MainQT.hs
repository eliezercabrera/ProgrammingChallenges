module Main where

import Control.Monad
import Data.List
import Data.List.Split
import System.Environment
import Data.Time
import qualified QuadTree as Q
import qualified Data.ByteString.Char8 as B

import Data.ByteString.Lex.Fractional
import Data.Maybe

sumC (Q.Coordinate x y) (Q.Coordinate x1 y1) = (Q.Coordinate (x + x1) (y + y1))

toCoords = map toTuple . chunksOf 2
    where toTuple [c1, c2] = (Q.Coordinate c1 c2)
               
origin :: Q.Coordinate
origin = (Q.Coordinate 0.0 0.0)

processNextPoint :: (Q.QuadTree, Q.Coordinate, Q.Coordinate)
                  -> Q.Coordinate
                  -> (Q.QuadTree, Q.Coordinate, Q.Coordinate)
processNextPoint (qt, c1, c2) c
    | currentDistance > Q.euclideanDistanceApprox c closestToNew = (newQT, c, closestToNew)
    | otherwise = (newQT, c1, c2)
    where currentDistance = Q.euclideanDistanceApprox c1 c2
          newQT = Q.insert c qt
          closestToNew = Q.findClosest c qt

main :: IO ()
main = do
    size <- read . head <$> getArgs
    
    (c1:c2:coordinates) <- toCoords . map (fst . fromJust . readSigned readDecimal) . B.words <$> B.getContents
    print $ foldl sumC (Q.Coordinate 0.0 0.0) (c1:c2:coordinates)
    start <- getCurrentTime
    let seed = (Q.insert c1 (Q.insert c2 $ Q.QuadTree size $ Q.empty origin 10.0), c1, c2)
        (_, myHome, grandma's) = foldl processNextPoint seed coordinates
    putStrLn $ show myHome ++ " " ++ show grandma's
    print $ Q.euclideanDistance myHome grandma's
    end   <- getCurrentTime
    print $ diffUTCTime end start