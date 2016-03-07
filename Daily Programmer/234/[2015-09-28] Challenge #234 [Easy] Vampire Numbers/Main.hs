module Main where

import Data.Function (on)
import Control.Monad (replicateM)
import System.Environment (getArgs)
import Data.List (intercalate, sort, sortBy, nubBy, (\\), (!!))

import Data.Time

toIntList :: Int -> [Int]
toIntList = reverse . go
    where go n = case quotRem n 10 of
                  (0, 0) -> []
                  (intInit, intLast) -> intLast : go intInit 

subsequencesOfSize :: Int -> [a] -> [[a]]
subsequencesOfSize n xs 
    | n > size  = []
    | otherwise = subsequencesBySize xs !! (size - n)
    where size = length xs
          subsequencesBySize [] = [[[]]]
          subsequencesBySize (x:xs)
              = let next = subsequencesBySize xs
                in zipWith (++) ([]:next) (map (map (x:)) next ++ [[]])
                  
vampireNumbers :: Int -> Int -> [(Int, [Int])]
vampireNumbers noDigits noFangs
    = let noFangDigits = noDigits `div` noFangs
      in  sortBy (compare `on` fst)
              [(vampire, fangs)
              | fangs <- subsequencesOfSize noFangs [10^(noFangDigits - 1)..10^noFangDigits - 1]
              , and (zipWith (<) fangs (tail fangs))
              , let vampire = product fangs
              , let vampireList = toIntList vampire
              , let fangLists = map toIntList fangs
              , length vampireList == noDigits
              , null (foldl (\\) vampireList fangLists)]

prettyPrint :: Int -> [Int] -> String
prettyPrint vampire fangs
    = show vampire ++ " = " ++ intercalate "*" (map show fangs)
                        
main :: IO ()
main = do
    [numberOfDigits, numberOfFactors] <- map read <$> getArgs
    let results = vampireNumbers numberOfDigits numberOfFactors
    start <- getCurrentTime
    putStr . unlines . map (uncurry prettyPrint) $ results
    end <- getCurrentTime
    print $ diffUTCTime end start