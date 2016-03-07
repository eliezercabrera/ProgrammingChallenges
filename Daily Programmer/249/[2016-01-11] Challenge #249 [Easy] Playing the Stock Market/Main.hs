import Data.Ord (comparing)

import qualified Data.ByteString.Char8 as B

import Data.Vector.Generic (convert)
import qualified Data.Vector           as V
import qualified Data.Vector.Unboxed   as U

import Data.Attoparsec.ByteString.Char8

import Data.Time.Clock.POSIX
import Control.DeepSeq

buyLowSellHigh :: U.Vector Double -> (Double, Double)
buyLowSellHigh prices
    = let size
              = U.length prices - 2
          profit (buy, sell)
              = sell - buy
          tails
              = V.iterateN (size - 2) U.tail . U.tail . U.tail
          candidates
              = V.zipWith
                      (\buy sells -> U.map ((,) buy) sells)
                      (convert prices)
                      (tails prices)
      in  V.maximumBy (comparing profit)
        $ V.map (U.maximumBy (comparing profit)) candidates

main :: IO ()
main = do
    input <- B.getContents
    
    let parser = sepBy double space

    let prettyPrint (buy, sell)
            = show buy ++ " " ++ show sell

    t0 <- getPOSIXTime
            
    case feed (parse parser input) B.empty of
        Done _ prices
            -> do let vector = U.fromList prices
                  t1 <- deepseq vector getPOSIXTime
                  putStrLn $ prettyPrint (buyLowSellHigh vector)
                  t2 <- getPOSIXTime
                  putStrLn $ "Best Pair: "  ++ show (t2 - t1)
                  putStrLn $ "Total Time: " ++ show (t2 - t0)
        _
            -> putStrLn "Malformed input. "