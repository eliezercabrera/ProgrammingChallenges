module Main where

import Data.Maybe (fromJust)
import Data.Char (isUpper, isAlpha)
import Data.Tuple.Utils (fst3, thd3)
import Data.List (delete, find, sort, (\\))
import qualified Data.Map as M

stableMatch :: [String] -> [(Char, Char)]
stableMatch xs = M.toList . thd3 $ until (null . fst3) match (men, map head women, M.empty)
    where (men, women) = (filter (isUpper . head) xs, xs \\ men)
          match (((currentMan:topWoman:restChoices):restMen), unpairedWomen, pairs)
              | topWoman `elem` unpairedWomen
                  = (restMen
                    , delete topWoman unpairedWomen
                    , M.insert topWoman currentMan pairs)
              | rank currentMan topWoman < rank rival topWoman
                  = (restMen ++ [rivalChoices]
                    , unpairedWomen
                    , M.adjust (const currentMan) topWoman pairs)
              | otherwise
                  = ((currentMan:restChoices):restMen
                    , unpairedWomen
                    , pairs)
               where rank m w = fromJust $ length . takeWhile (/= m) <$> find ((== w) . head) women
                     rival = pairs M.! topWoman
                     rivalChoices = fromJust $ find ((== rival) . head) men              

main :: IO ()
main = interact $ unlines . sort . map format . stableMatch . map (filter isAlpha) . lines
    where format (w, m) = '(' : m : "; " ++ w : ")"