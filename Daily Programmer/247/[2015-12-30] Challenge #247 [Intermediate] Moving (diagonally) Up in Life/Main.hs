module Main where

import Control.Monad (guard)
import Data.List (elemIndices)
import Prelude hiding (Right)
import System.Environment
import Data.Time

data Move = Up | Right | UpRight

type Coordinates = (Int, Int)

move :: Move -> Coordinates -> Coordinates
move Up      (x, y) = (x    , y + 1)
move Right   (x, y) = (x + 1, y    )
move UpRight (x, y) = (x + 1, y + 1)

reachUpLeft :: Coordinates -> Int
reachUpLeft target@(x, y) = go (0, 0)
    where go coord@(x', y')
               | target == coord  = 1
               | x' > x || y' > y = 0
               | otherwise = sum [ go (move m coord)
                                 | m <- [Up, Right, UpRight]]

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
               
main :: IO ()
main = do
    [x] <- map read <$> getArgs
    start <- getCurrentTime
    print (reachUpLeft (x, x))
    end   <- getCurrentTime
    print (diffUTCTime end start)