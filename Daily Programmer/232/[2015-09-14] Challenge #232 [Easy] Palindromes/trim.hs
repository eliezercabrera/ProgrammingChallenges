import Data.List.Split
main = interact $ unlines . map head . chunksOf 10 . lines