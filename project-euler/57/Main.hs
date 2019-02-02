module Main where

import Data.Ratio

root :: Integer -> Rational
root 0 = 1 + 1 % 2
root n = 1 + 1 / (go n)
    where go :: Integer -> Rational
          go 1 = 2 + 1 % 2
          go m = 2 + (1 / (go (m - 1)))

longerDenominator :: Rational -> Bool
longerDenominator frac = length (show $ numerator frac)
                         > length (show $ denominator frac)
          
main :: IO ()
main = print . length . filter longerDenominator . map root $ [0 .. 999]