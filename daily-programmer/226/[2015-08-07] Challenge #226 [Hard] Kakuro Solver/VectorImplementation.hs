module Main where

import Data.Char (digitToInt, ord)
import Control.Monad (replicateM, guard)
import Data.List (nub, tails)
import Data.List.Split (chunksOf)
import qualified Data.Vector as V
import qualified Data.Set as S
import Data.Maybe (listToMaybe, fromMaybe, isNothing, fromJust)
import System.Environment (getArgs)

type Index = (Char, Int)
data Constraint = Constraint Int [Index] deriving (Eq, Show)

getIndices (Constraint _ indices) = indices

readConstraint :: String -> Constraint
readConstraint = rC . words
    where rC (s:ss) = Constraint (read s) (map readIndex ss)

readIndex :: String -> Index
readIndex [c, n] = (c, digitToInt n)

takeEvery :: Int -> V.Vector Int -> V.Vector Int
takeEvery n xs = V.map (xs V.!) $ V.fromList [0, n .. V.length xs - 1]

keepTaking :: Int -> V.Vector Int -> V.Vector (V.Vector Int)
keepTaking n xs | V.null xs = V.empty
                | otherwise = V.take n xs `V.cons` keepTaking n (V.drop n xs)
               
atIndex :: V.Vector (V.Vector Int) -> Index -> Int
atIndex xs (column, row) = let rIndex = row - 1
                               cIndex = ord column - ord 'A'
                               vRow = xs V.!? rIndex
                               cell = if isNothing vRow then error $ show rIndex ++ ' ' : show cIndex else (fromJust vRow) V.!? cIndex
                           in  if isNothing cell then error $ show rIndex ++ ' ' : show cIndex else fromJust cell

meetsConstraint :: V.Vector (V.Vector Int) -> Constraint -> Bool
meetsConstraint xs (Constraint s indices) = s == sum (map (xs `atIndex`) indices)

nub' :: V.Vector Int -> V.Vector Int
nub' = go S.empty
  where go s xs | V.null xs = V.empty
                | S.member (V.head xs) s = go s (V.tail xs)
                | otherwise    = V.head xs `V.cons` go (S.insert (V.head xs) s) (V.tail xs)
                
tails' :: V.Vector Int -> V.Vector (V.Vector Int)
tails' xs | V.null xs = V.empty
          | otherwise = xs `V.cons` tails' (V.tail xs)

buildPuzzles :: Int -> Int -> [Constraint] -> [V.Vector Int]
buildPuzzles col row constraints =
  [candidate | candidate <- V.replicateM (col*row) [1..9],
               let rows = keepTaking col candidate,
               let columns = V.map (takeEvery row) $ V.take col $ tails' candidate,
               let unique xs = V.length xs == V.length (nub' xs),
               V.all unique rows && V.all unique columns,
               all (meetsConstraint rows) constraints]

main :: IO ()
main = do
    arg <- fmap (fmap read . listToMaybe) getArgs
    [columns, rows] <- fmap (map read . words) getLine :: IO [Int]
    constraints <- fmap (map readConstraint . lines) getContents
    print . maybe id take arg $ buildPuzzles columns rows constraints