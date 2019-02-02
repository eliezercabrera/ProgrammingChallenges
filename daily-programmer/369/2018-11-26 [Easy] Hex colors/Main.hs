module Main where

import Control.Monad (guard)
import Data.Char (chr, intToDigit, isDigit, ord)

newtype Color =
  Color [Int]

makeColor :: [Int] -> Maybe Color
makeColor numbers = do
  guard (length numbers == 3)
  guard (all (\number -> number >= 0 && number <= 255) numbers)
  return (Color numbers)

type Line = String

readRgb :: Line -> Maybe Color
readRgb line = do
  let numbers = words line
  guard (all (all isDigit) numbers)
  makeColor (fmap read numbers)

decimalToHex :: Int -> String
decimalToHex = show . takeWhile (== (0, 0)) . scanl1 (`quotRem` 16)

printHexadecimalColor :: Color -> String
printHexadecimalColor (Color colors) = '#' : concatMap decimalToHex colors

main :: IO ()
main =
  interact $
  unlines .
  fmap (maybe "Not a valid color." printHexadecimalColor . readRgb) . lines
