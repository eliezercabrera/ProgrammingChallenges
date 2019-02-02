module Main where

import Data.List (intercalate, dropWhileEnd, intersperse, dropWhile, mapAccumL, findIndices)
import Data.Maybe (catMaybes)
import Data.List.Split (splitWhen)
import Safe (tailSafe, initSafe)
import System.Environment (getArgs)

data ParsingState = TextLine | FeatureSide | InsideFeature

parseChar :: ParsingState -> Char -> (ParsingState, Maybe Char)
parseChar TextLine      '+' = (FeatureSide,   Nothing)
parseChar TextLine      '|' = (InsideFeature, Nothing)
parseChar TextLine       c  = (TextLine,      Just c )
parseChar FeatureSide   '+' = (TextLine,      Nothing)
parseChar FeatureSide    _  = (FeatureSide,   Nothing)
parseChar InsideFeature '+' = (FeatureSide,   Nothing)
parseChar InsideFeature '|' = (TextLine,      Nothing)
parseChar InsideFeature  _  = (InsideFeature, Nothing)

data Side = LeftSide | RightSide
data Feature = Feature {side :: Side, content :: String}

extractFeature :: Side -> [String] -> String
extractFeature s ls
    = tailSafe $ initSafe $ concatMap (intercalate " ") $ intersperse [" "] $ paragraphs
          where featureBoxedLines = map tail $ takeWhile ((== '|') . head) ls
                featureLines = map takeUntilRightSide featureBoxedLines
                textLines = map (dropWhile (== ' ') . dropWhileEnd (== ' ')) featureLines
                paragraphs = splitWhen null $ map reverseOrNot textLines
                reverseOrNot = case s of
                                LeftSide  -> id
                                RightSide -> reverse

takeUntilRightSide :: String -> String
takeUntilRightSide line | takeWhile (/= '|') line /= line = takeWhile (/= '|') line
                        | otherwise = iterate (takeWhile (/= '+')) line !! 2

type LineNumber = Int
numberedFeatures :: [String] -> [(LineNumber, Feature)]
numberedFeatures = nFts 0
    where nFts _ [] = []
          nFts n (l:ls)
            | head l == '+' &&
              last l == '+' = (n, Feature LeftSide  (extractFeature LeftSide ls))
                              : (n, Feature RightSide (extractFeature RightSide (map reverse ls)))
                              : nFts (n + 1) ls
            | head l == '+' = (n, Feature LeftSide  (extractFeature LeftSide ls))
                              : nFts (n + 1) ls
            | last l == '+' = (n, Feature RightSide (extractFeature RightSide (map reverse ls)))
                              : nFts (n + 1) ls
            | otherwise = nFts (n + 1) ls
            
filterLine :: String -> String
filterLine = catMaybes . snd . mapAccumL parseChar TextLine

trimLine :: String -> String
trimLine = dropWhileEnd (`elem` " -") . dropWhile (== ' ')

concatLines :: String -> String -> String
concatLines text []   = init text
concatLines text " "  = init text ++ "\n\n"
concatLines text line =      text ++ line

decolumnize :: [String] -> String
decolumnize = init . foldl concatLines "" . map ((++ " ") . trimLine . filterLine)

annotateParagraphs :: [String] -> [LineNumber] -> [(LineNumber, Feature)] -> String
annotateParagraphs ls lns fs
    = init $ intercalate "\n\n" $ map (foldl concatLines "" . snd . unzip) $ foldr preprendFeature paragraphs fs
        where filteredInput = zip [0..] $ map ((++ " ") . trimLine . filterLine) ls
              paragraphs = splitWhen ((`elem` lns) . fst)  filteredInput
  
preprendFeature :: (LineNumber, Feature) -> [[(LineNumber, String)]] -> [[(LineNumber, String)]]
preprendFeature f@(n, feature) (p@(ph@(pn, pc):_):ps)
    = if pn <= n
        then ((pn, '(' : content feature ++ ") " ++ pc) : tail p) : ps
        else p : preprendFeature f ps

main :: IO ()
main = do 
    input     <- fmap (tail . lines) getContents
    arguments <- fmap (concat) getArgs

    let paragraphIndices = findIndices (all (== ' ')) $ map filterLine input
        features = filter (not . null . content . snd) $ numberedFeatures input

    if null arguments
      then putStr $ decolumnize input
      else putStr $ annotateParagraphs input paragraphIndices features