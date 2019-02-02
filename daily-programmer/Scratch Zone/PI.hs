{-# LANGUAGE ExtendedDefaultRules #-}
module Main where

import Conduit
import Data.Time
import System.Environment

main :: IO ()
main = do
  count <- read . head <$> getArgs
  start <- getCurrentTime
  successes <- sourceRandomN count $$ lengthIfC (\(x, y) -> x^2 + y^2 < 1)
  print $ successes / fromIntegral count * 4
  print . abs . diffUTCTime start =<< getCurrentTime