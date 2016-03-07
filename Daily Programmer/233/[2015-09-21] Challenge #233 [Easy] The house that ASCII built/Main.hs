module Main where

import Data.List (transpose, splitAt, findIndex)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)
import System.Random

type Box = [String]

basicBox = ["+---+", "|   |", "+---+"]
emptyBox = replicate 3 "     "

mergeBoxes :: Box -> Box -> Box
mergeBoxes a b = transpose $ verticalMerge $ map transpose [a,b]

toBoxes :: [Bool] -> [String] -> [[Box]]
toBoxes bs input = map (map charToBox) $ zipWith zip coins (map padLine input)
    where charToBox (  _  , '%') = putDoor basicBox
          charToBox (False, '*') = basicBox
          charToBox (True , '*') = putWindow basicBox
          charToBox (  _  , ' ') = emptyBox
          coins = chunksOf (length $ concat input) bs
          wide = maximum $ map length input
          padLine l = l ++ replicate (wide - length l) ' '

verticalMerge :: [Box] -> Box
verticalMerge = foldl1 mergeFloors
    where mergeFloors top (b:bs) = (init top) ++ zipWith mergeTile (last top) b : bs
          mergeTile ' ' c =  c
          mergeTile '+' '-' = '+'
          mergeTile '+' '+' = '-'
          mergeTile '-' _ = ' '
          mergeTile '|' '|' = ' '
          mergeTile c _ = c

putWindow :: Box -> Box
putWindow box@(a:(p:q:_:r:s):c)
    | box == basicBox = a:(p:q:'o':r:s):c
    | otherwise = box

putDoor :: Box -> Box
putDoor (a:(p:_:_:_:t):c) = a:(p:"| |"++t):c

main :: IO ()
main = do
  input <- lines <$> getContents
  coinFlips <- randoms <$> newStdGen
  doorPositon <- randomRIO (0, length (last input) - 1)
  let (left, (_:right)) = splitAt doorPositon (last input)
      doorInput = init input ++ [left ++ '%' : right]
  putStr . unlines . concat . map (foldl1 mergeBoxes) . toBoxes coinFlips $ doorInput -- . buildRoofs . verticalMerge . map (foldl1 mergeBoxes)
-- unlines . concatMap (foldl1 (zipWith (++))) . toBoxes