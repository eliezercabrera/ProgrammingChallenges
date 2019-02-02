import System.Random.Shuffle (shuffleM)

import Data.List.Split (chunksOf)
bonus = all (\chunk -> all (`elem` chunk) "OISZLJT") . chunksOf 7
main = putStrLn . show . bonus  . concat =<< mapM shuffleM (replicate 8 "OISZLJT")