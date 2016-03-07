import Control.Monad (msum)

search = bool Nothing (Just . head) null

example1 = search ["Hello", "this", "is", "dog"]
example2 = search ["hello", "this", "is", "dog"]
example3 = search []
example4 = search ["hH"]
example5 = search ["H"]

main =
  putStr $ unlines ["http://www.viz.com/anime/streaming/berserk-episode-1/" ++ show n ++ "/sub" | n <- [1..20000]]