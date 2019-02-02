import Data.Ord
import Data.List
import Data.Char
import Data.List.Split
import Data.Bool
import System.Random  

shuffle xs = head . filter (/= xs) . map go . tails
    where go = fst . unzip . sortBy (comparing snd) . zip xs

trueWords [] = []
trueWords (c:cs) = current : trueWords rest
    where (current, rest) = span (f . isAlpha) (c:cs)
          f = bool not id (isAlpha c)

typoglyfy ns = zipWith scramble ns . trueWords
    where scramble rs (c:cs)
              = bool (c:cs) (c : shuffle (init cs) rs ++ [last cs]) (isAlpha c && length cs > 2)

main = do
    rs <- chunksOf 20 . randomRs (1, 20) <$> newStdGen :: IO [[Int]]
    interact $ concat . typoglyfy rs