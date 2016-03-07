import Control.Monad (guard)
import Data.Maybe    (fromMaybe)
import Data.Ord      (comparing)
import Data.List     (sort, sortBy, group)
import Data.Char     (isAlpha, isLower, toLower, toUpper)

type Passage = String

topLetters :: Passage -> [Char]
topLetters
    = map head -- [Char]
    . reverse . sortBy (comparing length) . group -- [[Char]]
    . sort . map toLower . filter isAlpha -- [Char]

decode :: [Char] -> Passage -> Passage
decode topAlienLetters encodedText
    = map substitute encodedText
    where encodedToDecodedDict
              = zip (topLetters encodedText) topAlienLetters
          substitute c
              = fromMaybe c
              $ do guard (isAlpha c)
                   let preserveCase = if isLower c then toLower else toUpper
                   preserveCase <$> lookup (toLower c) encodedToDecodedDict
                  -- <$> is an infix operator for fmap

main :: IO ()
main = interact (decode "etasrinohldmucfypbwvgxkq")
-- interact :: (String -> String) -> IO ()
--              ^_ this function is applied to standard input