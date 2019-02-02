import Data.List (group)

input = "1113122113"
lookAndSay = concatMap (\g -> show (length g) ++ [head g]) . group