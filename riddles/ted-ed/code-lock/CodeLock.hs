module Main where

import Control.Monad (replicateM)
import Data.List (sort, nub)

main :: IO ()
main = print (length (nub (fmap sort (replicateM 4 [0..9]))))
