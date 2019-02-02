module Main where

import Codec.Picture.Repa (Img, readImageR, toUnboxed)
import qualified Data.Vector.Unboxed as V
import Data.List (genericLength)
import Data.List.Split (chunksOf)
import Data.Either (either)
import Data.Word (Word8)
import Control.Monad ((=<<))
import System.Environment (getArgs)

data Color = Black | White deriving Eq

wordToColor :: Word8 -> Color
wordToColor  0  = Black
wordToColor 255 = White

main :: IO ()
main = putStrLn . either id (show . estimatePI . imageToList) =<< readImageR . head =<< getArgs
      
imageToList :: Img a -> [[Color]]
imageToList img = let matrix = toUnboxed img
                      dimension = round . sqrt . fromIntegral $ V.length matrix
                      pixelList = map wordToColor $ V.toList matrix
                  in chunksOf dimension pixelList
    
estimatePI :: [[Color]] -> Double
estimatePI xs = let circle = map (filter (== Black)) $ filter (Black `elem`) xs
                    radius = genericLength circle / 2.0
                    area   = sum $ map genericLength circle
                in  area / (radius ^ 2)