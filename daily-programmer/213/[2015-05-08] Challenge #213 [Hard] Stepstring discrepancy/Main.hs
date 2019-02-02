module Main where

import Control.Monad
import Data.List
import Data.Ord
import Data.Time

import qualified Data.Vector.Unboxed as U

-- 6140s -- Original String implementation
-- 6036s -- Convert String to [Int]
-- 3.15s -- Abort when start point grows to large (theoretical max lower than current max)
-- 1.40s -- Also abort when step grows too large (theoretical max lower than current max)
-- 0.75s -- Keep track of string size (constant time vs linear)
-- 0.11s -- Change [Int] to Unboxed Vector Int (better locality of reference? faster stepString creation)
-- Quick enough

stringToInt :: String -> U.Vector Int
stringToInt = U.fromList . map (\c -> if c == 'a' then 1 else negate 1)

stepString :: U.Unbox a => U.Vector a -> Int -> Int -> U.Vector a
stepString s start step = let indices = [start, start + step .. U.length s - 1]
                          in U.fromList $ map (U.unsafeIndex s) indices

maxStepDistance :: U.Vector Int -> Int
maxStepDistance stri = mSD stri [1..U.length stri - 1] 0
    where mSD _ [] maxSD = maxSD
          mSD str (step:steps) maxSD = let start = [0..step-1]
                                       in  if ((U.length str) `div` step) + 1 < maxSD
                                             then maxSD
                                             else mSD str steps (currentStepStringMaxSD str step start maxSD)
          currentStepStringMaxSD _ _ [] maxSD = maxSD
          currentStepStringMaxSD str step (start:starts) maxSD =
                                       let stepStr = stepString str start step
                                       in  if ((U.length str - start) `div` step) + 1 < maxSD
                                            then maxSD
                                            else currentStepStringMaxSD str step starts (max maxSD (findMaxStepDistance stepStr))

findMaxStepDistance :: U.Vector Int -> Int
findMaxStepDistance str = fMSD str 0 0 0 0
    where fMSD ss p n mp mn = if U.null ss
                                then max mp mn
                                else let s = U.unsafeHead ss
                                         newp = max 0 (p + s)
                                         newn = max 0 (n - s)
                                     in  fMSD (U.unsafeTail ss) newp newn (max newp mp) (max newn mn)
                          

main :: IO ()
main = do
    strings <- getContents
    start <- getCurrentTime
    let maxStepDistances = fmap maxStepDistance $ fmap stringToInt $ lines strings
    mapM_ print maxStepDistances
    end <- getCurrentTime
    print $ diffUTCTime end start