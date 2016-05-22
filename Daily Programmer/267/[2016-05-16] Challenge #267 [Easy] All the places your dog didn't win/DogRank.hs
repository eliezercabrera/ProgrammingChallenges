module Main where

import Data.List

toCardinal :: Int -> String
toCardinal position
    = show position
   ++ case show (position `rem` 100 + 10) of
          ('2':_) -> "th"
          (_:"1") -> "st"
          (_:"2") -> "nd"
          (_:"3") -> "rd"
          _       -> "th"

printRanks position
    = intercalate ", "
    . map toCardinal
    . delete position

main :: IO ()
main = do
    position <- read <$> getLine
    putStr (printRanks position [1..110])