module Main where

import Data.List (delete, tails)
import Data.Char (digitToInt, isDigit)

type Frame = String
type Point = Int

frameToPoints :: Frame -> Point
frameToPoints frame
    | any (`elem` frame) "X/" = 10
    | otherwise = (sum . map digitToInt . delete '-') frame

breakUp10th :: Frame -> [Frame]
breakUp10th frame = take 3 . (++ repeat "")
    $ case frame of
          'X' : 'X' : _  -> map pure frame
          'X' : _        -> [head frame] : [ tail frame ]
          _   : _   : _  -> init frame   : [[last frame]]

calculateScore :: [Frame] -> Point
calculateScore realFrames
    = sum [addPoints x y z | (x:y:z:_) <- tails totalFrames]
    where totalFrames = init realFrames ++ breakUp10th (last realFrames)
          addPoints a b c = sum . map frameToPoints
              $ case a of
                    "X" -> case head b of
                        'X' -> case head c of
                            'X' -> [a, b, c]
                            _   -> [a, b, [head c]]
                        _   -> [a, b]
                    _ : "/" -> case head b of
                        '-' -> [a]
                        _   -> [a, [head b]]
                    _  -> [a]

main :: IO () 
main = interact $ unlines . map (show . calculateScore . words) . lines