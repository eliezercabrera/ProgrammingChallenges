module Main where

import Data.List       (transpose, genericLength, minimumBy)
import Data.Char       (isAlphaNum, toLower)
import Data.Ord        (comparing)
import Data.List.Split (chunksOf)
import Control.Monad   (guard)

encrypt :: String -> String
encrypt message'
    = unwords . transpose . chunksOf (snd $ leastArea candidates)
    $ message
    where message
              = filter isAlphaNum (map toLower message')
          leastArea
              = minimumBy (comparing $ uncurry (*))
          candidates
              = do let sqrtL = sqrt (genericLength message)
                   rows     <- [floor sqrtL .. ceiling sqrtL]
                   columns  <- [rows        .. ceiling sqrtL]
                   guard    (rows * columns >= genericLength message)
                   return   (rows, columns)
                   
main :: IO ()
main = interact $ unlines . map encrypt . lines