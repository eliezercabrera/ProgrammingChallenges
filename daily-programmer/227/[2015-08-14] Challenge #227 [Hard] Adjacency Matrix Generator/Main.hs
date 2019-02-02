-- Comment lines start with "--"

-- Notation:
-- [Integer] is a list of integers
-- String is a type synonym for [Char]

-- function foo bar would be written function(foo, bar) in most other languages
-- the type of this function might be:
-- function :: Foo -> Bar, a function named "function" that takes an element
-- of type Foo and returns an element of type Bar

-- Standard Functions:
-- map applies a function (1st arg) to a list (2nd argument)
-- <$> does the same (it is an operator, so it is used infix)
-- <$> can also be used to apply a function to types with context
-- i.e. [Int] is an Int that may have 0, 1, or many elements (non-determism context)
-- Maybe Int maybe be just and Int or nothing (it might be 0 or 1 elements)
-- IO Int is an Int obtained from performing an I/O action

-- Standard operators:
-- the dot operator (.) is function composition
-- the dolar sign ($) is function application, but with low priority
-- i.e. both sides of $ are evaluated before calling the function to
-- the left with the right side argument

module Main where

import Data.Array (Array, listArray, assocs, (!))
import Data.List (elem, delete, find, sortBy)
import Data.Char (intToDigit, isAlpha)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

-- these are type synonyms; e.g.: a Coordinate is literally a 2-tuple of Ints
type Coordinate = (Int, Int)
type Vertex = Char

-- This functions have type: Function :: Coordinate -> Coordinate
up    (y, x) = (y - 1, x    )
left  (y, x) = (y    , x - 1)
down  (y, x) = (y + 1, x    )
right (y, x) = (y    , x + 1)

-- allDirections is a list of functions. Functions truly are first-class citizens!
allDirections :: [Coordinate -> Coordinate]
allDirections     = [up , up . right, right, right . down, down, down . left, left, left . up]

directionsSymbols :: String
directionsSymbols = ['|', '/'       , '-'  , '\\'        , '|' , '/'         , '-', '\\'     ]

-- this "constant" is an association of directions with the symbol that represents
-- them in the input. It is a list of 2-tuples of functions and characters
directionsChars :: [(Coordinate -> Coordinate, Char)]
directionsChars = zip allDirections directionsSymbols

-- this function surrounds the whole input in a one-element thick empty space (' ') border
padInput :: [String] -> [String]
padInput s = padRows : paddedSides ++ [padRows]
    where -- length of longest line
          size = maximum $ map length s
          -- we prepend a white space character to the line, and we we prepend what results
          -- to an infinitely-long string of whitespace (repeat ' ')
          -- with take n we *take* only the first n Chars of the resulting infinitely-long String
          -- (++): string concatenation operator
          paddedSides = map ((' ':) . take (size + 1) . (++ repeat ' ')) s
          -- empty row (whitespace string)
          padRows = take (size + 2) (repeat ' ')

-- Array Coordinate Char is an array of characters that are indexed by coordinates (instead of ints)
traceVertex :: Array Coordinate Char -> Coordinate -> [Vertex]
traceVertex a c = neighbors
    where -- neighbors (adjatent vertices): just trace from the current vertex to all valid directions
          neighbors = map (trace c) . fst . unzip $ filter validDirection directionsChars
          -- check the adjacent character, and return true if it is the appropriate symbol for tracing
          -- into a certain direction
          validDirection (d, letter) = a ! (d c) == letter
          -- return the vertex if one is found, or change direction upon encountering '#',
          -- or just keep going
          trace coord direction
              -- if the we've reached a vertex, return it
              | isAlpha (a ! (direction coord)) = a ! direction coord
              -- keep tracing along if we haven't reached a spare vertex
              | a ! (direction coord) /= '#' = trace (direction coord) direction
              -- a spare vertex ('#') indicates that we need to change directions
              | a ! (direction coord) == '#' = changeDirection (direction coord) coord
          -- to change direction, check adjacent cells, and pick the one with a symbol that is both
          -- appropriate for the directions (i.e. '-' for down is not appropriate, '|' is) and that is not
          -- the direction we've just come from
          changeDirection coord oldCoord = trace (direction coord) direction
              where direction = fst . fromJust $ find differentDirection directionsChars
                    differentDirection (d, letter) = a ! (d coord) == letter && a ! (d coord) /= a ! (oldCoord)
                                                     -- Valid character         -- Not from whence we come

main :: IO ()
main = do
    -- getContents: return all the input as a String
    -- lines: returns a list of strings, each element of which is a line (it ended up with '\n')
    -- tail: return all but the first element of a list. We don't need the first line of the input
    input <- padInput . tail . lines <$> getContents
    
    -- Let's create an array with boundaries (1,1) to (height, wideness) of input
    -- the elements in this array are the characters in input
    -- concat takes a list of string an returns a string by concatenating each element
    let inputArray = listArray ((1, 1), (length input, length $ head input)) (concat input)
    
        -- assocs array returns a list of 2-tuple of indices (coordinates in our case)
           -- and elements (vertices in our case) of the array
        -- we sort this list by its vertices (snd returns second element in tuple)
        -- we only take from this sorted list the tuples whose second elements are letters (vertices)
        (coordinates, vertices) = unzip . filter (isAlpha . snd) $ sortBy (comparing snd) $ assocs inputArray
        
        -- for vertices 'a', 'b', and 'c', matrix would be ["abc", "abc", "abc"]
        matrix = replicate (length vertices) $ take (length vertices) ['a'..]
        
        -- is a list in which each element is the vertices obtained by tracing from a given vertex
        adjacencyLists = map (traceVertex inputArray) coordinates
        
        -- row by row, we covert to 0 if the vertex is not associated with the vertex that the row
           -- is associated with, and 1 if the vertex (Char) is associated
        -- 'a' `elem` someString returns true if 'a' is in someString
        -- backticks (`) are used to convert regular functions into infix operators
        
        output = zipWith (\cs row -> (\c -> if c `elem` cs then 1 else 0) <$> row) adjacencyLists matrix
    -- putStr prints a string to standard output, intToDigit converts an Int into a Char
    -- unlines is the opposite of lines (described above)
      -- someString = unlines (lines someString)
    putStr . unlines $ map intToDigit <$> output