module Main where

import Data.List (find)
import qualified Data.ByteString.Char8 as B

import Data.Time

spellCheck :: [B.ByteString] -> B.ByteString -> B.ByteString
spellCheck dictionary word
    | longestPrefix == word = word
    | otherwise = longestPrefix `B.append` ('<' `B.cons` suffix)
    where suffix = B.drop (B.length longestPrefix) word
          Just longestPrefix
              = find isPrefixInDictionary prefixes
          isPrefixInDictionary prefix
              = any (prefix `B.isPrefixOf`) dictionary
          prefixes
              = reverse (B.inits word)

main :: IO ()
main = do
    start <- getCurrentTime
    dictionary <- B.lines <$> B.readFile "enable1.txt"
    B.interact $ B.unlines . map (spellCheck dictionary) . B.lines
    end   <- getCurrentTime
    print $ diffUTCTime end start