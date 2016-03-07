module Main where

import Control.Monad (guard)
import Data.List (elemIndices)
import System.Environment (getArgs)
import Data.Time (getCurrentTime, diffUTCTime)

import Data.Array

type Coordinates = (Int, Int)

coordinates :: [String] -> Maybe [Coordinates]
coordinates ls
    = do let rawCoordinates = [ (x, y) | (y, line) <- zip [0..] ls
                                       , x <- elemIndices 'X' line]

         let diffCoordinates (x1, y1) (x2, y2) = (x2 - x1, y2 - y1)
         let hasNegativeCoordinate (x, y) = x < 0 || y < 0

         let result = zipWith diffCoordinates
                              rawCoordinates
                              (tail rawCoordinates)

         guard (not $ null rawCoordinates)
         guard (not $ any hasNegativeCoordinate result)
         return result

numberOfSteps :: [Coordinates] -> [Integer]
numberOfSteps coords
    = map (table !) coords
    where x = maximum (map fst coords)
          y = maximum (map snd coords)

          f i 0 = 1
          f 0 j = 1
          f i j = table ! (i - 1, j - 1)
                + table ! (i - 1, j    )
                + table ! (i    , j - 1)

          table = listArray bounds
                 [f i j | (i, j) <- range bounds]
          bounds = ((0, 0), (x, y))

main :: IO ()
main = do
    args <- getArgs
    start <- getCurrentTime
    case args of
        []     -> interact $ maybe "<invalid input>"
                                   (show . product . numberOfSteps)
                           . coordinates . reverse . tail . lines
        [x, y] -> print (product $ numberOfSteps [(read x, read y)])
    end <- getCurrentTime
    print (diffUTCTime end start)