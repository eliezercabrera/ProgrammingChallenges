module Main where

{- USE STATE ARRAY OF MAYBE INT AS DATA STRUCTURE, INDEXED BY INDEX -}

import Data.Char (digitToInt)
import Control.Monad (guard)
import Data.List (nub)
import Data.Maybe (listToMaybe, mapMaybe, catMaybes, isJust, isNothing)
import System.Environment (getArgs)
import qualified Data.Map as M

type Index = (Char, Int)
data Constraint = Constraint {target :: Int, indices :: [Index]} deriving (Eq, Show)

readConstraint :: String -> Constraint
readConstraint = rC . words
    where rC (s:ss) = Constraint (read s) (map readIndex ss)

readIndex :: String -> Index
readIndex [c, n] = (c, digitToInt n)

generateRest :: Int -> Int -> [Index] -> [[Int]]
generateRest s accum [] = [[s - accum]]
generateRest s accum (i:inds)
  = concat [ map (freeCell :) (generateRest s (accum + freeCell) inds)
            | freeCell <- [1 .. 9]
            , s >= accum + freeCell + length inds
            , ]

generateCandidates :: M.Map Index (Maybe Int) -> Constraint -> [M.Map Index (Maybe Int)]
generateCandidates mp (Constraint s inds)
  = [ currentMap
     | let setCells  = catMaybes $ map (mp M.!) $ filter (isJust . (mp M.!)) inds
     , let freeCells = filter (isNothing . (mp M.!)) inds
     , rest <- filter ((== length freeCells). length) $ generateRest s (sum setCells) (length freeCells)
     , let currentMap = foldl (\accum (key, val) -> M.adjust (\_ -> Just val) key accum) mp $ zip freeCells rest
     , let  currentMap]

buildPuzzles :: [Constraint] -> M.Map Index (Maybe Int) -> [M.Map Index (Maybe Int)]
buildPuzzles [] mp = [mp]
buildPuzzles (c:constraints) mp
  = concatMap (buildPuzzles constraints) (generateCandidates mp c)

main :: IO ()
main = do
    arg <- fmap (fmap read . listToMaybe) getArgs :: IO (Maybe Int)
    [columns, rows] <- fmap (map read . words) getLine :: IO [Int]
    constraints <- fmap (map readConstraint . lines) getContents
    let cells = M.fromList $ zip (nub $ concatMap indices constraints) (repeat Nothing) :: M.Map Index (Maybe Int)
    print $ map M.toList $ buildPuzzles constraints cells