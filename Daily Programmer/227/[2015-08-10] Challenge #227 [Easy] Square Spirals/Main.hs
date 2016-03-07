module Main where

import Data.Char (isDigit)

type Size = Int
type Dimension = Int
type Coordinate = (Dimension, Dimension)

fromCenterToCorner :: Size -> Coordinate -> Coordinate
fromCenterToCorner size (x, y) = let offset = size `div` 2 + 1
                                 in (x + offset, y + offset)

fromCornerToCenter :: Size -> Coordinate -> Coordinate
fromCornerToCenter size (x, y) = let offset = size `div` 2 + 1
                                 in (x - offset, y - offset)

getPoint :: Size -> Int -> Coordinate
getPoint size p = let k = ((floor $ sqrt (fromIntegral $ p - 1)) - 1) `div` 2 + 1
                  in  fromCenterToCorner size (min k (max (-k) $ abs (p - 4*k^2 - k - 1) - 2*k),
                                               min k (max (-k) $ abs (p - 4*k^2 + k - 1) - 2*k))

getPosition :: Size -> Dimension -> Dimension -> Int
getPosition size x' y' | y ==  k = (2*k + 1)^2 -   k + x
                       | x == -k = (2*k + 1)^2 - 3*k + y
                       | y == -k = (2*k + 1)^2 - 5*k - x
                       | x ==  k = (2*k + 1)^2 - 7*k - y
                       where (x, y) = fromCornerToCenter size (x', y')
                             k  = max (abs x) (abs y)

main :: IO ()
main = interact $ solve . map readInt . words
    where readInt s | all isDigit s = read s
                    | otherwise = error "Invalid input: Contains characters that are not numeric, a space, or a newline."
          solve [size, index] = show $ getPoint size index
          solve [size, x, y]  = show $ getPosition size x y
          solve xs = error $ case xs of
                               []  -> "Invalid input: The input is empty."
                               [_] -> "Invalid input: Only one integer argument was provided"
                               _   -> "Invalid input: Too many integers (" ++ show (length xs) ++ ") were provided."