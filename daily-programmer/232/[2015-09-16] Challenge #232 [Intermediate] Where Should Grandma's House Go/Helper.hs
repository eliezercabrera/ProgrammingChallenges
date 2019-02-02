import Data.List.Split
main = interact $ unlines . map unwords . chunksOf 2 . lines