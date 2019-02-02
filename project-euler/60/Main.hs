module Main where

import Data.List
import Data.Numbers.Primes

import Criterion.Main

twoDecompositon :: Integral int => int -> (int, int)
twoDecompositon = go 0
    where go e n
              | r == 1     = (e, n)
              | otherwise  = go (e + 1) q
              where (q, r) = n `quotRem` 2

euler :: Int -> [[Int]]
euler limit
    =
    let primes'   = takeWhile (<= limit) (tail primes)
    in  [ ps
        | a <- primes'
        , b <- filter (isPrime . concatInt a) $ dropWhile (< a) primes'
        , c <- filter (isPrime . concatInt b) $ dropWhile (< b) primes'
        , d <- filter (isPrime . concatInt c) $ dropWhile (< c) primes'
        , e <- filter (isPrime . concatInt d) $ dropWhile (< d) primes'
        , let ps = [a, b, c, d, e]
        , all (isPrime . uncurry concatInt) (pairs1 ps)]

pairs1 :: Eq a => [a] -> [(a, a)]
pairs1 xs = [ (x, y) | x <- xs
                    , y <- xs
                    , x /= y]

pairs2 :: [a] -> [(a, a)]
pairs2 = go []
    where go _ [] = []
          go xs (y:ys) = map ((,) y) (xs ++ ys) ++ go (y : xs) ys

pairs3 :: [a] -> [(a, a)]
pairs3 xs = [ (x, y) | (x : rest) <- tails xs, y <- rest]
          ++ [ (y, x) | (x : rest) <- tails xs, y <- rest]
                
concatInt :: Integral int => int -> int -> int
concatInt x y
    = x * 10^(go y 0) + y
    where go 0 n = n
          go a n = go (a `quot` 10) (n + 1)

main :: IO ()
main = 
    defaultMain
        [ bgroup "pairs1" [ bench "100 " $ nf pairs1 ([1 .. 100 ] :: [Int])
                          , bench "1000" $ nf pairs1 ([1 .. 1000] :: [Int])
                          , bench "1200" $ nf pairs1 ([1 .. 1200] :: [Int])
                          , bench "2000" $ nf pairs1 ([1 .. 2000] :: [Int])
                          , bench "4000" $ nf pairs1 ([1 .. 4000] :: [Int])]
        , bgroup "pairs2" [ bench "100 " $ nf pairs2 ([1 .. 100 ] :: [Int])
                          , bench "1000" $ nf pairs2 ([1 .. 1000] :: [Int])
                          , bench "1200" $ nf pairs2 ([1 .. 1200] :: [Int])
                          , bench "2000" $ nf pairs2 ([1 .. 2000] :: [Int])
                          , bench "4000" $ nf pairs2 ([1 .. 4000] :: [Int])]

        , bgroup "pairs3" [ bench "100 " $ nf pairs3 ([1 .. 100 ] :: [Int])
                          , bench "1000" $ nf pairs3 ([1 .. 1000] :: [Int])
                          , bench "1200" $ nf pairs3 ([1 .. 1200] :: [Int])
                          , bench "2000" $ nf pairs3 ([1 .. 2000] :: [Int])
                          , bench "4000" $ nf pairs3 ([1 .. 4000] :: [Int])]
        ]
    --print (sort p1 == sort p2)