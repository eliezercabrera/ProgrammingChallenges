module Main where

reverseN = read . reverse . show
isPalindrome n = show n == reverse (show n)

isLychrel = all (not . isPalindrome) . take 50 . tail . iterate reverseAdd
    where reverseAdd n = n + (reverseN n)
    
main :: IO ()
main = print . length . filter isLychrel $ [1 .. 9999]