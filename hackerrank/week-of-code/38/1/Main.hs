module Main where

import Control.Monad (replicateM)

main :: IO ()
main = do
    testCases <- readLn
    answers <- replicateM testCases $ do  [_, k, _] <- fmap read . words <$> getLine :: IO [Int]
                                          sections <- fmap read . words <$> getLine
                                          return (length (takeWhile (< k) (scanl (+) 0 sections)))
    putStr (unlines (fmap show answers))
                                          
