module Main where

import Data.List

dottie n = fmap fst . find (uncurry (==)) . pairs . flip iterate n
    where pairs xs = zip xs (tail xs)
    
main = print $ dottie 0.5 (\x -> 4 * x * (1 - x) )