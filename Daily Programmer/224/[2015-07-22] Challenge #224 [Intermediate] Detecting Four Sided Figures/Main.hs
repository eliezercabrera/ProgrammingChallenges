module Main where

import Control.Monad
import Data.List

type Row = Int
type Column = Int
type Grid = [[Char]]
type GridPoint = (Row, Column)

data Line = VertLine Column Row Row | HorizLine Row Column Column

isRectangle :: Grid -> [GridPoint] -> Bool
isRectangle grid [(r1, c1), (r2, c2), (r3, c3), (r4, c4)]
    = r1 == r2 && c2 == c3 && r3 == r4 && c4 == c1 &&
      r1 < r3 && c1 < c2 &&
      all (validSide grid) [HorizLine r1 c1 c2, HorizLine r3 c4 c3, VertLine  c1 r1 r4, VertLine  c2 r2 r3]

validSide :: Grid -> Line -> Bool
validSide grid (VertLine  col startRow endRow) = all (\i -> (/= ' ') $ grid !! i   !! col) [startRow .. endRow]
validSide grid (HorizLine row startCol endCol) = all (\i -> (/= ' ') $ grid !! row !! i  ) [startCol .. endCol]

allCorners :: Grid -> [GridPoint]
allCorners = concatMap (\(y, xs) -> zip (repeat y) (elemIndices '+' xs)) . zip [0..]

countRectangles :: Grid -> Int
countRectangles grid = length $ filter (isRectangle grid) $ replicateM 4 $ allCorners $ grid

main :: IO ()
main = interact $ show . countRectangles . pad . lines
    where pad xs = map (take (maximum $ map length xs)) $ map (++ repeat ' ') xs