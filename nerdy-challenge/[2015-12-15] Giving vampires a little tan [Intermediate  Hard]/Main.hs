    {-# LANGUAGE RecordWildCards   #-}
    {-# LANGUAGE ParallelListComp  #-}

    module Main where

    import Data.Function   (on)
    import Data.List       (findIndices, nub)
    import Data.Char       (isDigit, digitToInt)
    import Prelude hiding  (takeWhile)

    import Data.Attoparsec.Text
    import qualified Data.Text as T

    data Bomb = Bomb { xBomb :: Int
                     , yBomb :: Int
                     , range :: Int
                     , power :: Int} deriving (Eq, Show)

    data Vampire = Vampire { strength :: Int
                           , xVampire :: Int
                           , yVampire :: Int} deriving (Eq, Show)

    isLethalBlast :: Vampire -> Bomb -> Bool
    isLethalBlast (Vampire {..}) (Bomb {..})
        = let distanceApart = on max abs (xBomb - xVampire) (yBomb - yVampire)
          in  distanceApart <= range 
           && strength <= power - distanceApart

    type Maze = [String]

    canDrawStraightLine :: Maze -> Vampire -> Bomb -> Bool
    canDrawStraightLine maze (Vampire {..}) (Bomb {..})
        = all (\(x, y) -> maze !! y !! x /= '#')
        $ bresenhamLine (xBomb, yBomb) (xVampire, yVampire)

    type Coordinates = (Int, Int)

    bresenhamLine :: Coordinates -> Coordinates -> [Coordinates]
    bresenhamLine (xOrigin, yOrigin) (xTarget, yTarget)
        = [ (xOrigin + deltaX, yOrigin + deltaY)
          | (deltaX, deltaY) <- go (xTarget - xOrigin) (yTarget - yOrigin)]
        where go run rise
                  | run  <  0  = [(-x,  y) | (x, y) <- go (- run) rise]
                  | rise <  0  = [( x, -y) | (x, y) <- go run (- rise)]
                  | rise > run = [( x,  y) | (y, x) <- go rise run    ]
                  | otherwise
                      = zip [0 .. run] . map fst . iterate step $ (0, run `div` 2)
                  where step (y, error)
                            | error' < 0 = (y + 1, error' + run)
                            | otherwise  = (y    , error')
                            where error' = error - rise

    parseVampires :: Parser (Maze, [Vampire])
    parseVampires
        = do maze <- lines . T.unpack <$> takeTill (== '-')
             let vampires
                    = [ Vampire (digitToInt strength) x y
                      | (y, line) <- zip [0..] maze
                      , x <- findIndices isDigit line
                      | strength <- filter isDigit (concat maze)]
             return (maze, vampires)

    parseBombs :: Parser [Bomb]
    parseBombs
        = skipWhile (== '-') *> endOfLine *> sepBy parseBomb endOfLine
        where parseBomb = Bomb <$> decimal <* skipSpace
                               <*> decimal <* skipSpace
                               <*> decimal <* skipSpace
                               <*> decimal

    parseRecords :: Parser [((Maze, [Vampire]), [Bomb])]
    parseRecords = sepBy parseRecord (skipSpace *> takeWhile (== '-') *> endOfLine)
        where parseRecord = (,) <$> parseVampires <*> parseBombs

    vampiresKilled :: Maze -> [Vampire] -> [Bomb] -> Int
    vampiresKilled maze vampires bombs
        = length . nub
        $ [ v | v <- vampires, b <- bombs
              , isLethalBlast v b
              , canDrawStraightLine maze v b]

    main :: IO ()
    main = do
        (Right records) <- parseOnly parseRecords . T.pack <$> getContents
        let specialUncurry f ((x, y), z) = f x y z
        putStr . unlines . map (show . specialUncurry vampiresKilled) $ records