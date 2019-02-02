{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Main where

import Data.List (isPrefixOf)
import Data.List.Split (chunksOf)

import Data.Array.Repa
import Control.Monad.Identity (runIdentity)

import Data.Array.Repa.Stencil (Boundary (BoundConst))
import Data.Array.Repa.Stencil.Dim2 (stencil2, forStencil2, makeStencil2)

import Control.DeepSeq (deepseq)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Environment (getArgs)

import Prelude hiding (map, zipWith)

type Red   = Int
type Green = Int
type Blue  = Int

type RGBColor = (Red, Green, Blue)

parseInput :: String -> Array U DIM2 RGBColor
parseInput input
    | header == "P3"
        = fromListUnboxed (Z :. height :. width)
          [ (r, g, b)
          | line  <- imageData
          , [r, g, b] <- fmap (fmap read)
                       . chunksOf 3 . words
                       $ line]
    | otherwise
        = error "Missing header."
    where (header : rest)
              = lines input
          (dimensions : imageData)
              = let isComment = ("#" `isPrefixOf`)
                in  filter (not . isComment) rest
          [width, height]
              = fmap read (words dimensions)

sobel :: Array U DIM2 RGBColor -> Array U DIM2 Int
sobel image
    = runIdentity (return =<< computeP
    $ zipWith edge horizontal vertical)
    where grayscale
              = let grayAverage (x, y, z) = (x + y + z) `quot` 3
                in  map grayAverage image
          horizontal
              = forStencil2 (BoundConst 127) grayscale
              [stencil2| (-1) 0 1
                         (-2) 0 2
                         (-1) 0 1 |]
          vertical
              = forStencil2 (BoundConst 127) grayscale
              [stencil2| (-1) (-2) (-1)
                         0 0 0
                         1 2 1 |]
          edge h v
              = (round . sqrt . fromIntegral) (v^2 + h^2)
              
toPPM :: Array U DIM2 Int -> String
toPPM imageArray
    = unlines
    $ "P2"
    : unwords (fmap show [width, height])
    : fmap (unwords . fmap show)
           (chunksOf width $ toList imageArray)
    where Z :. height :. width = extent imageArray
          
main = do
    [n] <- fmap read <$> getArgs
    t0 <- getPOSIXTime
    --image <- parseInput <$> getContents
    let image = fromListUnboxed (Z :. n :. n) [ (r, g, b) | r <- [0..n - 1]
                                                          , g <- [0..n - 1]
                                                          , let b = 1]
    t1 <- seq image getPOSIXTime
    t2 <- seq (sobel image) getPOSIXTime
    putStrLn $ unwords ["Parsing input took:"  , show (t1 - t0)]
    putStrLn $ unwords ["Edge detection took:",  show (t2 - t1)]