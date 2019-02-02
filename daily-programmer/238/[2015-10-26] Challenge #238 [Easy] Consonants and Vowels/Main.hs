{-# LANGUAGE ParallelListComp #-}
import Data.Bool (bool)
import Data.List ((\\), (!!))
import Data.Char (isUpper, toUpper)
import System.Random (randomRs, newStdGen)

vowels :: String
vowels = "aeiou"

consonants :: String
consonants = ['a' .. 'z'] \\ vowels

generateWord :: String -> [Int] -> [Int] -> String
generateWord pattern cs vs
    = [ capitalization (letters !! i)
      | p <- pattern, c <- cs, v <- vs
      , let (i, letters) = bool (c, consonants) (v, vowels) (p `elem` "Cc")
      , let capitalization = bool toUpper id (isUpper p)]

main :: IO ()
main = do
    letterPattern <- getLine
    if not $ all (`elem` "CcVv") letterPattern
        then do putStrLn $ "Illegal characters: "
                        ++ filter (`notElem` "CcVv") letterPattern
                        ++ ". Please try again"
                main
        else do let randomIndices list = randomRs (0, length list - 1)
                cIndices <- randomIndices consonants <$> newStdGen
                vIndices <- randomIndices vowels     <$> newStdGen
                putStrLn (generateWord letterPattern cIndices vIndices)