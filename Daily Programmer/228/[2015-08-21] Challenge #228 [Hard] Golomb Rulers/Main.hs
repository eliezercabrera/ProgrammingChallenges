module Main where

import Data.Time
import Data.List.Split
import Control.Monad.ST (ST, runST)
import Control.Monad (when)
import Data.Array.ST (writeArray, readArray, runSTUArray, STUArray, newArray)
import qualified Data.IntSet as S
import Data.List (find, tails, inits)
import System.Environment (getArgs)

type Ruler = [Int]

prettyPrintRulers :: [Ruler] -> String
prettyPrintRulers rulers = let (l:ls) = map (unwords . map show) rulers
                           in  show (length $ head rulers) ++ '\t' : show (last $ head rulers)
                                ++ '\t' : l ++ '\n' : unlines (map ("\t\t"++) ls)
                         
select :: [Int] -> [(Int, [Int])]
select [] = []
select (x:xs) = (x,xs) : [ (y, x:ys) | (y,ys) <- select xs ]

hasDuplicates :: [Int] -> Bool
hasDuplicates [] = False
hasDuplicates (x:xs) = any (== x) xs || hasDuplicates xs

choose :: Int -> [Int] -> [[Int]]
choose 0 _  = [[]]
choose n xs = [ y : concat (choose (n - 1) ys) | (y, ys) <- select xs]

sublists :: [Int] -> [[Int]]
sublists = filter (not . null) . concatMap inits . tails

checkDiffList :: [Int] -> Int -> ST s Bool
checkDiffList xs n = do
    arr <- newArray (0, n) False
    go arr xs
    where go :: STUArray s Int Bool -> [Int] -> ST s Bool
          go _ [] = return True
          go a (y:ys) = do
              isMember <- readArray a y
              if isMember
                then return False
                else writeArray a y True >> go a ys
              
               
differences :: Int -> Int -> [[Int]]
differences order size
  = [ x | x <- choose (order - 1) [1.. order + 4]
        , sum x == size
        , head x < last x
        , let ys = map sum $ sublists x
        , runST (checkDiffList ys (sum x))]
                        
golombRulers :: Int -> [[Ruler]]
golombRulers order 
  = map (map (scanl (+) 0) . differences order) ([guess])
    where guess | order == 2 = 1
                | order == 3 = 3
                | otherwise = round $ - 598 + (8237*x)/15.0 - (18467*x^2)/90.0 + (321*x^3)/8.0
                       - (155*x^4)/36.0 + (29*x^5)/120.0 - x^6/180.0
                       where x = fromIntegral order
                                
main :: IO ()
main = do
    [n] <- map read <$> getArgs
    let (Just optimalRulers) = find (not . null) $ golombRulers n
    start <- getCurrentTime
    putStr $ prettyPrintRulers optimalRulers
    end <- getCurrentTime
    print $ diffUTCTime end start