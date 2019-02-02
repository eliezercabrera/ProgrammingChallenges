module Main where

import Data.List.Split (splitOn)
import Text.Read (readMaybe)
import Control.Monad (guard)
import Data.Maybe (mapMaybe)

type Numerator = Integer
type Denominator = Integer
type Fraction = (Numerator, Denominator)

readFraction :: String -> Maybe Fraction
readFraction s = do
    guard $ length (splitOn "/" s) == 2
    let [left, right] = splitOn "/" s
    numerator   <- readMaybe left
    denominator <- readMaybe right
    return (numerator, denominator)
    
gcd' :: Integral int => int -> int -> int
gcd' a 0 = a
gcd' a b = gcd b (a `mod` b)

addFractions :: Fraction -> Fraction -> Fraction
addFractions (n1, d1) (n2, d2) = (n1*d2 + n2*d1, d1*d2)

reduceFraction :: Fraction -> Fraction
reduceFraction (n, d) = let cd = gcd' n d
                        in  (n `div` cd, d `div` cd)
                        
challenge :: [Fraction] -> Fraction
challenge = reduceFraction . foldl addFractions (0, 1)

printFraction :: Fraction -> String
printFraction (n, d) = show n ++ "/" ++ show d

main :: IO ()
main = interact $ printFraction . challenge . mapMaybe readFraction . tail . lines