module Main where

import Data.Time
import Data.List (genericLength, find)
import Data.List.Split (chunksOf)
import Control.Monad (forever, when, replicateM_)

import Pipes
import qualified Pipes.Prelude as P

import Data.Numbers.Primes

diagonals
    = scanl (+) 1
    . concatMap (replicate 4)
    $ [2, 4..]

nextSpiral :: Double -> Pipe [Int] Int IO ()
nextSpiral percentage = do 
    loop (0, 1)
    where loop (n', d')
            = do corners <- await
                 let ps = length (filter isPrime corners)
                 let (n, d) = (n' + ps, d' + 4)
                 let newPercentage = fromIntegral n / fromIntegral d
                 when (newPercentage <= percentage) 
                      (yield ((d + 1) `div` 2))
                 loop (n, d)
    
euler p = runEffect $ each (chunksOf 4 . tail $ diagonals)
                    >-> nextSpiral p 
                    >-> P.take 1 >-> P.print
      
measure :: IO () -> IO NominalDiffTime
measure f = do
    start <- getCurrentTime
    f
    end   <- getCurrentTime
    return (diffUTCTime end start)
                      
main :: IO ()
main = do
    print =<< measure (euler 0.1)