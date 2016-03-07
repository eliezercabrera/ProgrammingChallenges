    {-# LANGUAGE RecordWildCards   #-}

    module Main where

    import Data.Ratio
    import Control.Applicative
    import Data.Ord  (comparing)
    import Data.Char (isUpper, isDigit, isLower)
    import Data.List (nub, (!!), delete, sortBy, dropWhileEnd, splitAt, groupBy)

    import qualified Data.Text as T
    import qualified Data.Attoparsec.Text as P

    data Equation = Equation { leftSide  :: Expression
                             , rightSide :: Expression} deriving Show

    newtype Expression = Expression { molecules    :: [Molecule]} deriving Show
    newtype Molecule   = Molecule   { submolecules :: [SubMolecule]} deriving Show

    data SubMolecule = Simple   { element   :: Element
                                , subscript :: Subscript}
                     | Compound { submoleculesC :: [SubMolecule]
                                , subscript     :: Subscript} deriving Show

    type Element   = T.Text
    type Subscript = Int

    isOpenningBracket = (`elem` "([{")
    insideBrackets parseOperation
        =   P.char '(' *> parseOperation <* P.char ')'
        <|> P.char '[' *> parseOperation <* P.char ']'
        <|> P.char '{' *> parseOperation <* P.char '}'
    string = P.string . T.pack

    parseEquation :: P.Parser Equation
    parseEquation = do
      left <- parseExpression
      string " -> "
      right <- parseExpression
      return $ Equation left right

    parseExpression :: P.Parser Expression
    parseExpression = Expression <$> P.sepBy1 parseMolecule (string " + ")

    parseMolecule :: P.Parser Molecule
    parseMolecule = Molecule <$> P.many' parseSubmolecule

    parseSubmolecule :: P.Parser SubMolecule
    parseSubmolecule = do
      c <- P.peekChar
      case c of
        Just c | isOpenningBracket c -> parseCompound
        _ -> parseSimple
        

    parseSimple :: P.Parser SubMolecule
    parseSimple = do
      element <- parseElement
      subscript <- P.takeWhile isDigit
      if T.null subscript
        then return $ Simple element 1
        else return $ Simple element (read $ T.unpack subscript)

    parseCompound :: P.Parser SubMolecule
    parseCompound = do
      simples <- insideBrackets (P.many' parseSubmolecule)
      subscript <- P.takeWhile isDigit
      if T.null subscript
        then return $ Compound simples 1
        else return $ Compound simples (read $ T.unpack subscript)

    parseElement :: P.Parser Element
    parseElement = do
      capital <- P.satisfy isUpper
      rest <- P.takeWhile isLower
      return $ capital `T.cons` rest

    countMolecules :: Equation -> Int
    countMolecules (Equation {..}) = sum $ length . molecules <$> [leftSide, rightSide]
        
    elements :: Equation -> [Element]
    elements eq
        = nub . concatMap getMoleculeElements
        $ molecules (leftSide eq) ++ molecules (rightSide eq)
        where getMoleculeElements = concatMap getElements . submolecules
              getElements (Simple   {..}) = [element]
              getElements (Compound {..}) = concatMap getElements submoleculesC
              
    countElement :: Equation -> Element -> [Int]
    countElement (Equation left right) e = countSide left ++ map negate (countSide right)
        where countSide = map countMolecule . molecules
              countMolecule = sum . map countSubmolecules . submolecules
              countSubmolecules (Simple {..})
                  | e == element = subscript
                  | otherwise = 0
              countSubmolecules (Compound {..})
                  = sum $ map ((subscript*) . countSubmolecules) submoleculesC

    type Vector = [Rational]
    type Row    = [Rational]
    type Matrix = [[Rational]]

    type RowIndex = Int

    -- Without sorting, the matrix returned wouldn't be in triangular form
    -- Why? Zeroing the first element of a row might zero more cells
    gauss :: Matrix -> Matrix
    gauss = toUpperTriangular . map unitizeRowPivot . gauss' 0
        where toUpperTriangular = sortBy (comparing $ length . takeWhile (== 0))
              gauss' rowIndex matrix
                | rowIndex == length matrix = matrix
                | all (== 0) (matrix !! rowIndex) = gauss' (rowIndex + 1) matrix
                | otherwise = gauss' (rowIndex + 1) newPivotMatrix
                where newPivotMatrix = foldr (zeroRowPivot rowIndex) matrix otherIndices
                      otherIndices   = delete rowIndex [0 .. length matrix - 1]

    -- This function is ugly because I am using lists, which don't easily support mutation
    -- of particular elements
    -- This functions uses the row specified by the first argument to make the first element
    -- of the row given by the second argument equal to 0
    zeroRowPivot :: RowIndex -> RowIndex -> Matrix -> Matrix
    zeroRowPivot pivotRow targetRow matrix
      = up ++ (zipWith (+) oldRow scaledRow) : down
        where scaledRow = map (*scaleFactor) $ matrix !! pivotRow
              (up, (oldRow:down)) = splitAt targetRow matrix
              scaleFactor = negate $ nonZeroLead targetRow / nonZeroLead pivotRow
                  where leadingZeroes = takeWhile (== 0) (matrix !! pivotRow)
                        nonZeroLead = head . drop (length leadingZeroes) . (matrix !!)
              

    -- Scales elements in the row so that its first non-zero element becomes one
    unitizeRowPivot :: Row -> Row
    unitizeRowPivot row
        | all (== 0) row = row
        | otherwise = zipWith (*) row (repeat multiplicativeInverse)
          where multiplicativeInverse = 1 / pivot row
                pivot = head . dropWhile (== 0)

    showBalancedEquation :: String -> [Integer] -> String
    showBalancedEquation s' ns'
        | any (<= 0) ns' = show ns' --"Nope!"
        | otherwise = sBE (words s') ns'
        where sBE [molecule] [1] = molecule
              sBE [molecule] [n] = show n ++ molecule
              sBE (molecule:symbol:rest) (n:ns)
                = number ++ molecule ++ ' ' : symbol ++ ' ' : sBE rest ns
                  where number | n /= 1 = show n
                               | otherwise = ""

    balanceEquation :: T.Text -> Equation -> String
    balanceEquation eqText equation
        = let -- each row represents how many times an element apears
              -- on every molecule (on every "addend")
              matrix = map fromIntegral . countElement equation <$> elements equation
              -- discard last rows that are all zeroes, take the additive
              -- inverse of last element in rows
              pivots = map (negate . last . dropWhileEnd (== 0)) . dropWhileEnd (all (== 0)) $ gauss matrix
              -- if we have less pivots than molecules, we pad the
              -- pivots at the end with 1s
              paddedPivots = pivots
                ++ replicate (countMolecules equation - length pivots) (fromIntegral 1)
              -- the common denominator of the pivots is the least
              -- common multiple of their denominators
              commonDenominator = foldl lcm 1 $ map denominator paddedPivots
              -- we must have whole molecules, so let's get rid of the fractions
              wholePivots = map ((commonDenominator % 1)*) paddedPivots
              -- use the pivots we computed to annotated the input
              -- (the input is the string representing the chemical equation)
          in  showBalancedEquation (T.unpack eqText) (map numerator wholePivots)

    main :: IO ()
    main = do
        let processEquation line
                = either id (balanceEquation line) $ P.parseOnly parseEquation line
        interact $ unlines . map (processEquation . T.pack) . lines