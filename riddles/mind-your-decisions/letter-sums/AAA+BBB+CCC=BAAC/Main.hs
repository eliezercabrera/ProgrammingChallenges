module Main where


main :: IO ()
main = putStr . unlines $
    [ "A = " ++ show a ++ ", B = " ++ show b ++ ", C = " ++ show c
    | a <- [0..9]
    , b <- [0..9]
    , c <- [0..9]
    , a /= b
    , b /= c
    , a /= c
    , let aaa = a*111
    , let bbb = b*111
    , let ccc = c*111
    , aaa + bbb + ccc == 1000*(b) + 100*(a) + 10*(a) + 1*(c)
    ]
