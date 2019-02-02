module Main where

import System.Environment (getArgs)
import Data.Time (getCurrentTime, diffUTCTime)
-- ord takes a character and return an integer (e.g., ASCII table positon)
import Data.Char (ord)
-- Delete first occurance of element (Char) from list (String)
import Data.List (delete)
import Control.Monad (guard, (=<<))

langfordWords :: Int -> [String]
-- Just calling lf on a blank string the size of the langford words to find
-- and the first letters of the alphabet corresponding to the langford order
langfordWords n = lf (replicate (n*2) ' ') (take n ['A'..])

-- The first String argument represents the rest of the langford string
-- that is currently being computed. A blank character means that there
-- is no character at a previous position that would, per definition,
-- require a copy of it to occupy that space. It starts blank and full
-- of possibilities, so we must fill-in the characters.
-- The second String keeps track of which letters have been used
-- in the currently being computed langford word
lf :: String -> String -> [String]
-- If there are no more letters to add to the current langford, return
-- the rest of characters (we have filled every blank by now)
lf word [] = [word]
-- If the first character is blank, we need to choose a character from
-- the second string to form part of the current lanford word
lf (' ':xs) remainingLetters = do
    -- In fact, we pick every character in the second String as candidates
    currentLetter <- remainingLetters
    -- The current space, then, would be the character chosen. This is the
    -- first occurrence of the character, so we need compute how far away
    -- the second occurrence must be placed. If you are unfamiliar with
    -- Haskell, this notation means that for each letter in remaining letters,
    -- we will perform the following:
    let index = ord currentLetter - ord 'A' + 1
    -- The offset, or index, needs to be checked for possible out-of-bounds
    -- If OK, check that the space where the second occurrence should be
    -- is free (blank). For those unfamiliar with Haskell, the guard makes it
    -- so that the computation for the currently picked character stops if
    -- the condition does not hold.
    guard $ index < length xs && xs !! index == ' '
    -- Place the second ocurrance. In many other languages, this would be:
    -- xs[index] = currentLetter
    let xss = take index xs ++ currentLetter : drop (index + 1) xs
    -- Finally, we prepend the chosen character to the rest of the strings,
    -- which are computed with recursive calls to lf with the rest of the
    -- candidate langford words (for which we filled the second occurrence),
    -- and, naturally, we have one fewer letter to worry about for the
    -- second argument.
    -- Again, for those unfamiliar, map we will prepend the currentLetter
    -- to all the rest of the lanford candidates
    map (currentLetter:) $ lf xss (delete currentLetter remainingLetters)
-- If the first character isn't blank, then it has already being filled.
-- So we just add it to langford word we are computing. Every non-blank
-- character put into the first string argument was placed to us after
-- we checked the necessary conditions, so we can just add now without
-- checking.
lf (x:xs) remainingLetters = map (x:) $ lf xs remainingLetters

-- The do notation and the arrows (<-) in this function are different than
-- in the list monad we used before. Before it meant "for every character
-- in this string do the following," here it means "perform this IO action
-- and bind the result to this variable." It doesn't seem intuitive, for
-- beginners, but it isn't complicated and makes perfect sense once one
-- understand monads (not necessary for this example)
main :: IO ()
main = do
    -- n is the command line argument (integer). Get args return a list,
    -- but we only need/have one argument (head returns first element in
    -- list) and read makes the element an integer (things are read are
    -- strings)
    n <- fmap (read . head) getArgs
    -- Similar to getTimeOfDay
    start <- getCurrentTime
    -- Print the first 10 langforWords computed. They are computed in
    -- lexicographical order, so there is no need to sort them. Normally,
    -- list are printed like ["ABCA", "ABAC", "BCAB"...]. With unlines,
    -- every element is printed to a different line, and there are no
    -- brackets or quotes
    putStr $ unlines $ take 10 (langfordWords n)
    end   <- getCurrentTime
    -- print how long the computation took to run
    print $ diffUTCTime end start