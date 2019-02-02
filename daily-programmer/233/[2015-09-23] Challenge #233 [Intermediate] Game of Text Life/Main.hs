import Data.List.Split (chunksOf)
import Data.List (transpose, subsequences)
import System.Random (newStdGen, randomRs)

type Grid = [String]

gameOfLife :: [Int] -> Grid -> Grid
gameOfLife ns grid
  = chunksOf width $ zipWith mergeRule ns (transpose (map shift directions))
      where width   = length (head grid)
            deadRow = replicate width ' '
            up    = tail . (++ [deadRow])
            right = map (init . (' ' :))
            down  = init . (deadRow :)
            left  = map (tail . (++ " "))
            shift direction = concat (direction grid)
            directions = [v . h | v <- [id, up, down], h <- [id, left, right]]

mergeRule :: Int -> String -> Char
mergeRule randomNumber (cell:neighbors)
  = case length survivors of
      2 | isAlive cell -> newCellKind
      3 | isAlive cell -> cell
        | otherwise    -> newCellKind
      _                -> ' '
    where survivors   = filter isAlive neighbors
          newCellKind = survivors !! (randomNumber `mod` length survivors)
          isAlive = (/= ' ')

main :: IO ()
main = do
    randomSequence <- randomRs (1, foldl1 lcm [1..8]) <$> newStdGen
    interact $ unlines . gameOfLife randomSequence . padGrid . lines

padGrid :: Grid -> Grid
padGrid grid = map padLine grid
    where padLine line = take width (line ++ repeat ' ')
          width = maximum (map length grid)