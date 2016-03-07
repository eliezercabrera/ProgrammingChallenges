import Data.Char          (digitToInt)
import System.Environment (getArgs)

isSelfDescriptive :: Int -> Bool
isSelfDescriptive n
    = n == read (concatMap countOccurences [0 .. length ns - 1])
    where countOccurences
              = show . length . flip filter ns . (==)
          ns
              = fmap digitToInt (show n)

nDigitSelfDescriptive :: Int -> [Int]
nDigitSelfDescriptive n
    = filter isSelfDescriptive
    . filter ((== n) . digits)
    $ [10^(n - 1) .. 10^n - 1]
    
digits 0
    = 0
digits n
    = r + digits q
    where (q, r) = n `quotRem` 10

main :: IO ()
main = do
    [n] <- fmap read <$> getArgs
    mapM_ print (nDigitSelfDescriptive n)