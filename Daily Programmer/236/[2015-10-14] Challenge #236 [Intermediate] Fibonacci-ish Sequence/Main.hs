module Main where

fibs :: Int -> [Int]
fibs n = 0 : scanl (+) n (fibs n)

divides :: Int -> Int -> Bool
a `divides` b = b `rem` a == 0

minimumFibonaccish :: Int -> [Int]
minimumFibonaccish 0 = [0]
minimumFibonaccish n
    = takeWhile (<= n) fibonaccishSequence
    where fibonaccishSequence = fibs seed
          seed = n `div` (last evenDivisors)
          evenDivisors = filter (`divides` n) fibonacciSequence
          (_:fibonacciSequence) = takeWhile (<= n) (fibs 1)

main :: IO ()
main = interact
     $ unlines
     . map (unwords . map show . minimumFibonaccish . read)
     . lines