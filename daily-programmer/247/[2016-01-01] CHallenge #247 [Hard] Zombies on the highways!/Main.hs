import Data.Array.Unboxed

import Data.List       ((!!), find, nub, foldl')
import Data.List.Split (chunksOf)

import Data.Time.Clock.POSIX (getPOSIXTime)

type Place = Int
type Weight = Int

type Graph = UArray (Int, Int) Weight

buildGraph :: [(Int, Int, Int)] -> Graph
buildGraph placeData
    = let (xs, ys, _)
              = unzip3 placeData
          (minIx, maxIx)
              = (minimum places, maximum places)
              where places = xs ++ ys
          size = (2 * maxIx + 1)
      in listArray ((0, 0), (size, size))
      [ w | i <- [minIx .. size]
          , j <- [minIx .. size]

          , let isSameEdge (x, y, _)
                    =     (x, y)           == (i, j)
                       || (y, x)           == (i, j)
                       || (x, step y)      == (i, j)
                       || (step x, step y) == (i, j)
                       || (step y, step x) == (i, j)
                    where step n = n + maxIx + 1

          , let infinity
                    = maxBound `quot` 2 - 1
          , let third (_, _, x)
                    = x
          , let p `xor` q
                    = (p || q) && (not (p && q))
          , let w | i == j
                      = 0
                  | otherwise
                      = maybe infinity
                              (if (j > maxIx) `xor` (i > maxIx)
                                   then const 0
                                   else third)
                              (find isSameEdge placeData)]

shortestPath :: Graph -> (Weight, [Place], Int)
shortestPath graph
    = ( totalWeight
      , nub $ source : route ++ [destination]
      , size `quot` 2)
    where size
              = snd (snd $ bounds graph)
          (source, destination)
              = (0, size)
          totalWeight
              = table ! (source, destination)
          route
              = path source destination size

          table
              = foldl' go graph [1 .. size - 1]
              
          go t k
              = listArray bounds
                          [ min (t ! (i, j))
                                ( (t ! (i, k))
                                + (t ! (k, j)))
                          | (i, j) <- range bounds]
              where bounds = ((0, 0), (size, size))

          path _ _ 0
              = []
          path i j k
              = let direct
                        = table ! (i, j)
                    step
                        = table ! (i, k)
                        + table ! (k, j)
                in if direct < step
                   then path i j (k - 1)
                   else path i k (k - 1)
                     ++ [k]
                     ++ path k j (k - 1)

prettyPrint :: (Weight, [Place], Int) -> String
prettyPrint (zombiesAmount, places, maxIx)
    = unlines (zipWith printEdge places $ tail places)
    ++ "Reached Last Chance, encountering "
    ++ show zombiesAmount
    ++ " zombies in "
    where printEdge source destination
              = show' source
                   ++ isBlast ++ " to "
                   ++ show' destination ++ ","
              where isBlast
                        = if destination > maxIx
                             && source < 100
                             then " *BLAST*"
                             else ""
                    show' location
                        = show (location `rem` succ maxIx)

main :: IO ()
main = do
    start <- getPOSIXTime
    interact $ prettyPrint 
             . shortestPath
             . buildGraph
             . read . ('[':) . (++"]")
    end   <- getPOSIXTime
    print (end - start)