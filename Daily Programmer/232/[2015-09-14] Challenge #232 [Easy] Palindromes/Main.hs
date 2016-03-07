import Control.Monad
import Data.Char
import Data.List
import Data.Time
import qualified Data.Map.Strict as M

strip :: String -> String
strip = filter (\x -> x >= 'a' && x <= 'z') . map toLower

-- lines is smart enough to strip \n, but not smart enough to strip \r :P
chomp :: String -> String
chomp = filter (\x -> x /= '\r' && x /= '\n')

isPalindrome :: Eq a => [a] -> Bool
-- Slow implementation - I dislike it
isPalindrome []     = True
isPalindrome [x]    = True
isPalindrome (x:xs) = x == last xs && isPalindrome (init xs)


-- -- Main solution
-- main = do
--   str <- liftM (strip . concat . lines) getContents
--   putStrLn $ if isPalindrome str
--              then "Palindrome"
--              else "Not a palindrome"

-- The rest of this is for the bonus solution

-- Blindly mashing strings together is slow. Let's organize a bit.
-- At a bare minimum, every palindrome must begin and end with the same character.
-- One way of optimizing this is to make a map from each character to the complete
-- list of words that end with that character.
-- This would eliminate most of the possibilities for each pass.
-- But, we can do even better.
-- Suppose instead we made a map based on the last two characters.
-- Then we'd miss one-character words, but that's just "a" and "I"
-- So let's drill down to two character suffixes and check "a" or "I" manually.
-- Technically enable1.txt does not include "a" or "I" but I want to be thorough.
buildMap :: [String] -> M.Map (Char, Char) [String]
buildMap = foldr addToMap M.empty . filter ((>1) . length)
  where addToMap s m = let cs = chomp s
                       in M.insertWith (++) (last cs, last $ init cs) [cs] m

main = do
  wordlist <- liftM lines getContents
  let lastmap = buildMap wordlist
  let pairs = do
        w1 <- wordlist
        guard $ ((head w1, head $ tail w1) `M.member` lastmap)
                || (toLower (head w1) == 'a')
                || (toLower (head w1) == 'i')
        w2 <- (M.findWithDefault [] (head w1, head $ tail w1) lastmap) ++ ["a", "i"]
        guard $ w1 /= w2
        guard . isPalindrome . strip $ w1 ++ w2
        return . chomp $ w1 ++ " " ++ w2
  start <- getCurrentTime
  print . length $ pairs
  print . flip diffUTCTime start =<< getCurrentTime