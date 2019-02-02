module Main where

import Data.List
import Data.List.Split

data Puzzle = Empty | Full { letter :: Char
                           , up     :: Puzzle
                           , right  :: Puzzle
                           , down   :: Puzzle
                           , left   :: Puzzle} deriving (Eq, Show)

singleton c = Full c Empty Empty Empty Empty

insert 
insertString Empty dir (c:word) = Full {
                           
generatePuzzle :: [String] -> Puzzle
generatePuzzle = foldl merge Empty

merge :: Puzzle -> String -> Puzzle
merge p s = p

main :: IO ()
main = interact $ unlines . splitOn ","