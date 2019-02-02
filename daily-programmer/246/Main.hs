
{-# LANGUAGE RecursiveDo #-}

import Control.Applicative
import Data.Char
import Data.Foldable
import Text.Earley

grammar :: Grammar r (Prod r String Char String)
grammar = mdo
  word <- rule $ (:) <$> alphabetP <*> (word <|> pure [])
  return word

alphabetP :: Prod r String Char Char
alphabetP = asum (map letterP ['A'..'Z'])

letterP :: Char -> Prod r String Char Char
letterP c = c <$ word digits where
  digits = show (ord c - ord 'A' + 1)

main :: IO ()
main = interact (unlines . fst . fullParses (parser grammar))             

--main :: IO ()
--main = interact $ unlines . letterSplits