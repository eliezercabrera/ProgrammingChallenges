import Data.List (genericLength, nub)

shanonEntropy :: String -> Double
shanonEntropy string
    = (negate 1) * sum (map indexScore
                           (nub string))
    where indexScore c
              = (count / n) * logBase 2 (count / n)
              where count
                        = genericLength
                        $ filter (== c) string
                    n
                        = genericLength string

main :: IO ()
main
    = interact $ unlines . map (show . shanonEntropy) . lines