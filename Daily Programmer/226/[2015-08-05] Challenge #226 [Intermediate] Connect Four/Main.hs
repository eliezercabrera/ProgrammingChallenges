module Main where

import Control.Monad.ST (ST, runST)
import Control.Monad (foldM, replicateM, unless)
import Data.Array.ST (writeArray, STArray, newArray, freeze)
import Data.Array (Array, assocs, Ix, (!), elems)
import Data.Char (ord, toLower, isUpper, isLetter)
import Data.Maybe (mapMaybe, isJust, fromJust, fromMaybe, listToMaybe, isNothing)
import Data.List (find, nub, intercalate)

data Column = A | B | C | D | E | F | G deriving (Eq, Show, Ord, Enum, Ix)
data Chip = X | O deriving (Eq, Show, Ord, Ix, Enum)

type Space = Maybe Chip
type Index = (Column, Int)
type Move  = (Chip, Column)

hasWon :: Array Index Space -> Bool
hasWon = isJust . winningLine

winningLine :: Array Index Space -> Maybe (Chip, [Index])
winningLine arr = listToMaybe $ mapMaybe (\c -> fmap ((,) c) $ playerWinningLine c chips) [X, O]
    where chips = filter (isJust . snd) $ assocs arr
        
playerWinningLine :: Chip -> [(Index, Space)] -> Maybe [Index]
playerWinningLine c chips =
    let playerChips = fst $ unzip $ filter ((== c) . fromJust . snd) chips
        candidates = filter ((== 4) . length) . map nub . replicateM 4 $ playerChips
        isHorizontal line@((column, row):_) = line == zip (take 4 [column..]) (repeat row)
        isVertical   line@((column, row):_) = line == zip (repeat column) (take 4 [row..])
        isDiagonal   line@((column, row):_) = line == zip (take 4 [column..]) (take 4 [row..]) ||
                                              line == zip (take 4 [column..A]) (take 4 [row..])
    in  find (\x -> isHorizontal x || isVertical x || isDiagonal x) candidates
      
play :: [Move] -> ST s (Maybe (Int, Chip, [Index]))
play moves = do
    arr  <- newArray ((A, 1), (G, 6)) Nothing
    iArr <- freeze =<< foldM performMove arr moves
    return $ fmap (countMoves iArr) (winningLine iArr)

countMoves :: Array Index Space -> (Chip, [Index]) -> (Int, Chip, [Index])
countMoves arr (chip, indices)
    = (length $ filter (\space -> isJust space && fromJust space == chip) $ elems arr, chip, indices)
        

performMove :: (STArray s Index Space) -> Move -> ST s (STArray s Index Space)
performMove arr (chip, column) = do
    iArr <- freeze arr
    let index = find (isNothing . (iArr !)) $ zip (repeat column) [1..6]
    unless ((isJust . winningLine) iArr && isJust index) $ writeArray arr (fromJust index) (Just chip)
    return arr

readColumn :: Char -> Maybe Column
readColumn c | toLower c `elem` ['a'..'g'] = Just $ toEnum (ord (toLower c) - ord 'a')
             | otherwise = Nothing

readMove :: Char -> Maybe Move
readMove c = do
    column <- readColumn c
    let chip = if isUpper c then X else O
    return (chip, column)

showIndices :: Chip -> [Index] -> String
showIndices c = intercalate " " . map showIndex
    where showIndex (column, position) = (f $ show column) ++ show position
          f = map (if c == X then id else toLower)
               
main :: IO ()
main = do 
    moves <- return . mapMaybe readMove . filter isLetter =<< getContents
    putStrLn $
      case runST (play moves) of
        Just (moves, player, indices) -> show player ++ " won at move " ++ show moves ++
                                         " (with " ++ showIndices player indices ++ ")"
        Nothing -> "There was no winner."