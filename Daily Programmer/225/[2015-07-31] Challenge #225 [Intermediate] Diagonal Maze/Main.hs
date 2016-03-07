module Main where

import Data.List (elemIndices, all, nub, splitAt, iterate, span, intercalate)
import Data.List.Split (splitPlaces, chunksOf)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.String.Utils (replace)
import Control.Monad ((=<<))
import Control.Monad.Omega

type Coordinate = (Int, Int)

main :: IO ()
main = interact $ diagonalize . tail . lines

addChar :: [(Int, String)] -> (Int, Int, Char) -> [(Int, String)]
addChar xs (y, x, c) = map (\(n, line) -> if n == y + x then (n, c:line) else (n, line)) xs

replaceChar :: Char -> Char
replaceChar '-' = '\\'
replaceChar '|' = '/'
replaceChar  c  = c

spandLine :: String -> String
spandLine line = replace "|-" "| -" $ replace "-|" "- |" line -- concat . zipWith (++) bigraphs $ map (flip replicate ' ') [1..]
                 
padMaze :: [String] -> [String]
padMaze ls = let width = maximum $ map length $ map spandLine ls
                 
                 (top, bottom) = splitAt (length ls `div` 2) $ map spandLine ls
                 padline line n = replicate (n - (ceiling $ fromIntegral n / 3.0)) ' ' ++ line
                 topHalf = zipWith padline top [length top, length top - 1..]
                 bottomHalf =  zipWith padline bottom [0 .. length bottom - 1]
             in  map (\l -> replicate ((width - (length l)) ) ' ' ++ l) $ map spandLine ls --topHalf ++ bottomHalf

--diagonalize :: String -> [(Int, Int, Char)]
diagonalize s = diagonalMaze
    where mazeSideLength = length s
          indexedMaze = concatMap (\(y, line) -> map (\(x, c) -> (y, x, c)) line) $ zip [0..] $ map (zip [0..]) s
          emptyRotatedMaze = zip [0..] $ replicate (2*mazeSideLength - 1) ""
          rotatedMaze = snd . unzip $ foldl addChar emptyRotatedMaze indexedMaze
          filteredMaze = filter (not . elem '+') . padMaze $ rotatedMaze
          diagonalMaze = unlines $ (padMaze rotatedMaze) ++ map (map replaceChar) filteredMaze