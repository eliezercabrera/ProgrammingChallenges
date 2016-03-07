module Threes where

type Adjust = Int

threes :: Integral int => [Maybe (int, Adjust)]
threes 1 = [[1]]
threes n | n > 1 =
  let (q, r) = (n + 1) `divMod` 3
  in [n, 1 - r] : threes q

main :: IO ()
main = interact $ unlines . map (unwords . map show) . threes . read