module Main where

import Data.List as L
import Data.Ord
import Data.ByteString.Char8 as B

isVowel :: Char -> Bool
isVowel c = B.any (c ==) $ B.pack "aeiou"

consonantGrouping :: B.ByteString -> Bool
consonantGrouping = L.any ((>= 4) . B.length) . splitWith isVowel

vowelGrouping :: B.ByteString -> Bool
vowelGrouping = L.any ((> 3) . B.length) . splitWith (not . isVowel)

isGibberish :: B.ByteString -> Bool
isGibberish line = let cGCoefficient = L.any consonantGrouping     $ B.words line
                       vGCoefficient = L.any vowelGrouping         $ B.words line
                       nVCoefficient = L.any (not . B.any isVowel) $ B.words line
                   in nVCoefficient || vGCoefficient || cGCoefficient

main :: IO ()
main = B.interact $ B.unlines . L.filter (not . isGibberish) . B.lines