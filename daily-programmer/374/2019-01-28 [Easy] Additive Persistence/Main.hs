import Control.Monad (guard)
import Data.List (unfoldr)

digits :: Int -> [Int]
digits =
  unfoldr
    (\n -> do
       guard (n /= 0)
       let (q, r) = n `quotRem` 10
       return (r, q))

additivePersistence :: Int -> Int
additivePersistence = length . takeWhile (>= 10) . iterate (sum . digits)
