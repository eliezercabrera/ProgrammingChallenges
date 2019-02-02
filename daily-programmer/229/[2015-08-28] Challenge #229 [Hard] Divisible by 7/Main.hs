module Main where

import System.Environment
import Data.List
import Data.Time

fastIntegerReverse n = fIR n 0
    where fIR 0 result = result
          fIR number result = let (q, r) = number `quotRem` 10
                              in fIR q (10*result + r)

solution :: Int -> Integer
solution n = sum' [ x | x <- [0, 7.. 10^n], fastIntegerReverse x `mod` 7 == 0]

sum' :: [Int] -> Integer
sum' = foldl1' (+) . map toInteger

main = do 
    start <- getCurrentTime
    print . solution =<< fmap (read . head) getArgs
    print . abs . diffUTCTime start =<< getCurrentTime