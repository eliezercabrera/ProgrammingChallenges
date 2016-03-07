{-# LANGUAGE MultiParamTypeClasses #-}

module Main where

import Data.Array
import Data.Function (on)
import System.IO.Unsafe (unsafePerformIO)

import qualified Data.List as L
import qualified Data.IntMap.Strict as M
import qualified Data.Map.Strict as Q
import qualified Data.PQueue.Prio.Min as PQ

import Data.Time.Clock.POSIX (getPOSIXTime)

type Place = Int
type Weight = Int

type Edges = M.IntMap Weight
type Graph = Array Place Edges

edgeWeight :: Graph -> Place -> Place -> Weight
edgeWeight graph source
    = ((graph ! source) M.!)
    
neighbors :: Graph -> Place -> [Place]
neighbors graph
    = fst . unzip . M.toList . (graph !)

buildGraph :: [(Int, Int, Int)] -> Graph
buildGraph placeData
    = accumArray (flip (uncurry M.insert)) M.empty (minIx, 2*maxIx + 1) assocList
    where (xs, ys, _) = unzip3 placeData
          (minIx, maxIx) = let places = xs ++ ys
                           in  (minimum places, maximum places)
          assocList
                = let stackEdges y w = [(y, w), (y + maxIx + 1, 0)]
                      step n = n + maxIx + 1
                      explodeEdge (x, y, w)
                          = [ (x, (y, w)), (y, (x, w))
                            , (x, (step y, 0)) , (y, (step x, 0))
                            , (step x, (step y, w)) , (step y, (step x, w))]
                  in concatMap explodeEdge placeData

class PriorityQueue pq where
    extractMin :: Ord key => pq key val -> Maybe (val, pq key val)
    insert     :: Ord key => key -> val -> pq key val -> pq key val
    update     :: Ord key => key -> val -> pq key val -> pq key val
    singleton  :: Ord key => key -> val -> pq key val

instance PriorityQueue Q.Map where
    extractMin = Q.minView
    insert     = Q.insert
    update key val = Q.adjust (const val) key
    singleton = Q.singleton
    
shortestPath :: Graph -> Maybe ([Place], Weight)
shortestPath graph
    = loop priorityQ distanceSet previousSet
    where infinity = maxBound :: Int
          
          places = L.sort (indices graph)
          (source, destination) = (head places, last places)
          
          distances   = 0 : repeat infinity
          distanceSet = M.fromList (zip places distances)
          
          previousList = repeat Nothing
          previousSet  = M.fromList (zip places previousList)

          priorityQ = singleton 0 source :: Q.Map Weight Place
          loop pq ds ps = do
               (place, pq') <- extractMin pq
               if place == destination
                  then foundLoop destination []
                  else do let updateNeighbor (ds', ps', pq'') neighbor
                                  | alt < ds' M.! neighbor
                                      = ( M.adjust (const alt )      neighbor ds' 
                                        , M.adjust (const (Just place)) neighbor ps'
                                        , insert alt neighbor pq'')
                                  | otherwise
                                      = ( ds', ps', pq'')
                                  where alt = ds' M.! place + edgeWeight graph place neighbor
                          let (newDistanceSet, newPreviousSet, newPQ)
                                = foldl updateNeighbor (ds, ps, pq') (neighbors graph place)
                          loop newPQ newDistanceSet newPreviousSet
               where foundLoop location stack = do
                       case ps M.! location of
                         Just previousPlace
                            -> foundLoop previousPlace (location : stack)
                         Nothing
                            -> let weight = ds M.! destination
                               in  return (stack, weight)

prettyPrint :: ([Place], Weight) -> String
prettyPrint (places, zombiesAmount)
    = unlines (zipWith printEdge (0 : places) places)
    ++ "Reached Last Chance, encountering "
    ++ show zombiesAmount
    ++ " zombies in "
    where printEdge source destination
              = show source ++ " to " ++ show destination ++ ","
                               
main :: IO ()
main = do
    start <- getPOSIXTime
    interact $ maybe "Cannot reach Last Chance." prettyPrint 
             . shortestPath
             . buildGraph
             . read . ('[':) . (++"]")
    end   <- getPOSIXTime
    print (end - start)