module Main where

--import qualified Data.Array as A
import Data.Array
import Data.Ratio
import Data.Maybe
import Data.Monoid

type Resistance = Integer
type Edge = (Char, Char, Resistance)

instance Integral a => Monoid (Ratio a) where
    mempty = 0%1
    mappend = (+)

readEdge :: String -> Edge
readEdge
    = (\[a, b, c]
          -> (head a, head b, read c))
    . words

type Index = (Char, Char)
type AdjancencyMatrix = Array Index (Maybe Rational)

createAdjacencyMatrix :: Char -> [Edge] -> AdjancencyMatrix
createAdjacencyMatrix lastNode edges
    = fmap ((1/) <$>)
    $ accumArray (<>) Nothing bounds
                 [ ((min i j, max i j), Just (1 % resistance))
                 | (i, j, resistance) <- edges]
    where bounds = (('A', 'A'), ( lastNode, lastNode))
    
totalResistance :: AdjancencyMatrix -> Double
totalResistance
    = fromRational . sum . catMaybes . elems

main :: IO ()
main = do
  lastNode <- last <$> getLine
  edges <- fmap readEdge . lines <$> getContents
  let adjacencyMatrix = createAdjacencyMatrix lastNode edges
  print $ totalResistance adjacencyMatrix