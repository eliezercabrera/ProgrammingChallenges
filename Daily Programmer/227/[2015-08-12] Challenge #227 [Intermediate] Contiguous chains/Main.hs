module Main where

import Data.Array.ST (writeArray, STUArray, newArray, readArray)
import Control.Monad.ST (ST, runST)
import qualified Data.IntMap.Strict as M
import qualified Data.IntSet as S

type Vertex = Int
type Table a = M.IntMap a
type Graph = Table S.IntSet
type Edge = (Vertex, Vertex)
type Bounds = (Vertex, Vertex)

vertices :: Graph -> [Vertex]
vertices = M.keys

buildG :: [Edge] -> Graph
buildG = M.fromListWith S.union . map (\(v1, v2) -> (v1, S.singleton v2))

bounds :: Graph -> Bounds
bounds g = (fst $ M.findMin g, fst $ M.findMax g)

data Tree a = Node a (Forest a)
type Forest a = [Tree a]

dfs :: Graph -> [Vertex] -> Forest Vertex
dfs g vs = prune (bounds g) (map (generate g) vs)

dff :: Graph -> Forest Vertex
dff g = dfs g (vertices g)

generate :: Graph -> Vertex -> Tree Vertex
generate g v = Node v (map (generate g) (S.toList $ g M.! v))

type Set s = STUArray s Vertex Bool

prune :: Bounds -> Forest Vertex -> Forest Vertex
prune bs ts = runST $ do m <- newArray bs False
                         chop m ts
                         
chop :: Set s -> Forest Vertex -> ST s (Forest Vertex)
chop m [] = return []
chop m (Node v ts : us) = do 
    visited <- readArray m v
    if visited
      then chop m us
      else do writeArray m v True
              xs <- chop m ts
              ys <- chop m us
              return $ Node v xs : ys

readNodes :: String -> [Vertex]
readNodes = fst . unzip . filter ((/= ' ') . snd) . zip [0..]

buildGraph :: Int -> [Vertex] -> Graph
buildGraph l vertices = buildG $ concatMap vertexToEdges vertices
    where vertexToEdges vertex = zip (repeat vertex) (neighbors vertex)
          neighbors x = x : filter (`S.member` S.fromList vertices) adjacent
              where adjacent = case (x `mod` l) of
                                  0 -> [x + 1, x + l, x - l]
                                  r -> if r == l - 1
                                         then [x - 1, x + l, x - l]
                                         else [x - 1, x + 1, x - l, x + l]

main :: IO ()
main = do
    l <- read . last . words <$> getLine
    vertices <- readNodes . concat . lines <$> getContents
    print . length . dff $ buildGraph l vertices