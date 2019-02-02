    import Data.List (sort)
    import Control.Monad (replicateM_, unless)
    import System.Random.Shuffle (shuffleM)

    matches rs = (++ " in the correct position.") . go . length . filter id . zipWith (==) rs
      where go 0 = "No treasure is"
            go 1 = "Only one treasure is"
            go 2 = "Only two treasures are"

    respond 0 _  = putStrLn "You have failed! Indiana was crushed by the ceiling!"
    respond n rs = do
        ans <- concat . words <$> getLine
        case ans == rs of
          True -> putStrLn "You did it! Indiana managed to escape the room!"
          _ | sort rs /= sort ans -> putStrLn "You don't have such treasures."
            | otherwise -> putStrLn (matches rs ans ++
                if n == 1 then [] else " You hear the ceiling coming down.") >> respond (n - 1) rs
                                  
    main = do
        putStrLn "Time is running out and the ceiling is falling on you!"
        respond 7 treasures =<< shuffleM "abcd"