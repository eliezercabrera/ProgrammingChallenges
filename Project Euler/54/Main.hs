module Main where

import Control.Monad (guard)
import Data.List (sort, group, nub, splitAt)
import Data.Char (digitToInt)
import Data.Maybe (listToMaybe, catMaybes)

data Suit  = Clubs | Spades | Hearts | Diamonds
             deriving (Eq, Ord, Show)

data Value = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
           | Jack | Queen | King | Ace
           deriving (Eq, Ord, Enum, Show)

data Card = Card {value :: Value, suit :: Suit}
            deriving (Eq, Ord)

data Play = HighCard Value
           | OnePair Value
           | TwoPairs Value Value
           | ThreeOfAKind Value
           | Straight Value
           | Flush
           | FullHouse Value Value
           | FourOfAKind Value
           | StraightFlush Value
           | RoyalFlush
           deriving (Eq, Ord, Show)
           
readSuit 'C' = Clubs
readSuit 'S' = Spades
readSuit 'H' = Hearts
readSuit 'D' = Diamonds
readSuit  _  = error "Invalid Suit."

readValue 'T' = Ten
readValue 'J' = Jack
readValue 'Q' = Queen
readValue 'K' = King
readValue 'A' = Ace
readValue  n  | n `elem` ['2'..'9'] = toEnum (digitToInt n - 2)
              | otherwise = error "Invalid Value."
              
readHands line = let (h1, h2) = splitAt 5 (words line)
                     readCard [v, s] = Card (readValue v) (readSuit s)
                 in  (map readCard h1, map readCard h2)
              
type Hand = [Card]

highCard = HighCard . maximum . map value

onePair hand = do
    let vals = sort $ map value hand
    [(val:_)] <- pure $ filter ((== 2) . length) (group vals)
    return (OnePair val)

twoPairs hand = do
    let vals = sort $ map value hand
    [(val1:_), (val2:_)] <- pure . reverse
                         $ filter ((== 2) . length) (group vals)
    return (TwoPairs val1 val2)

threeOfAKind hand = do
    let vals = sort $ map value hand
    [(val:_)] <- pure $ filter ((== 3) . length) (group vals)
    return (ThreeOfAKind val)

straight hand = do
    let values = sort $ map value hand
    let minValue = head values
    guard (values == take 5 [minValue..])
    return (Straight minValue)

flush hand = do
    guard (null . tail . nub . map suit $ hand)
    return Flush

fullHouse hand = do
    (OnePair pairVal) <- onePair hand
    (ThreeOfAKind tripleVal) <- threeOfAKind hand
    return (FullHouse tripleVal pairVal)

fourOfAKind hand = do
    let vals = sort $ map value hand
    [(val:_)] <- pure $ filter ((== 4) . length) (group vals)
    return (FourOfAKind val)

straightFlush hand = do
    flush hand
    (Straight minValue) <- straight hand
    return (StraightFlush minValue)

royalFlush hand = do
    (StraightFlush Ten) <- straightFlush hand
    return RoyalFlush

data Player = P1 | P2 | Tie
            deriving (Eq, Show)
            
play hand1 hand2
    = let plays hand
              = head . catMaybes . map ($ hand) $
                  [royalFlush, straightFlush, fourOfAKind, fullHouse
                  , flush, straight, threeOfAKind, twoPairs, onePair, Just . highCard]
      in (case compare (plays hand1) (plays hand2) of
            GT -> P1
            LT -> P2
            EQ -> case compare (reverse . sort . map value $ hand1)
                               (reverse . sort . map value $ hand2) of
                    GT -> P1
                    LT -> P2
                    EQ -> Tie)
    
main :: IO ()
main = interact $ show . length . filter (== P1) . map (uncurry play . readHands) . lines