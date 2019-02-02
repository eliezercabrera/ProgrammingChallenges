import Data.List
import Control.Monad (replicateM, forever)
import Data.Ord
import System.Random.Shuffle
import System.Random.Mersenne.Pure64
import MonadMT

type Name   = String
type Family = [Name]

secretSanta :: PureMT -> [Family] -> [Name]
secretSanta generator families
    = flip evalRandom generator loop
    where loop
           = do assignment <- map fst . sortBy (comparing snd) . zip participants
                           <$> sequence (replicate (length participants) getInt)
                if (any (`isInfixOf` assignment) forbiddenPairs)
                   then loop
                   else return assignment
          participants   = nub (concat families)
          forbiddenPairs = concatMap (replicateM 2) families

prettyPrint :: [Name] -> String
prettyPrint participants
    = unlines $ zipWith printPair participants (tail participants ++ participants)
    where printPair a b = a ++ " -> " ++ b

main :: IO ()
main = do
    gen <- newPureMT
    interact $ prettyPrint . secretSanta gen . map (words) . lines
