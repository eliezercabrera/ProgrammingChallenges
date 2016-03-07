    -- Comments start with two scores
    -- Function application is done with spaces
    -- What in many languages is "add(1,2)", in Haskell is "add 1 2"
    -- Function application has priority over
    -- boolean and mathematical operators

    module Main where

    import Data.List (permutations, (!!))
    import System.Random (randomRIO)

    -- Recursive definition of factorial function
    
    -- Type declaration: the factorial function takes one Int argument
    -- and returns and Int
    factorial :: Int -> Int
    -- Function definitions. Very close to mathematical notation.
    factorial 0 = 1
    factorial 1 = 1
    factorial n = n * factorial (n - 1)

    -- For impure "variables" that require IO (i.e. inputList), <- is used
    -- For pure "variables" that don't require IO, a let binding is used.

    -- The IO type represents side effects. It is a Monad, but that doesn't
    -- matter in this example.
    main :: IO ()

    -- Using "do" notation allows us to use a notation similar to that used
    -- in imperative languages.
    main = do
      -- Read the single line of input as a list of strings using the getLine function
      -- The "words" function returns a list containing the substrings that were
      -- delimited by blank spaces (' ') on the input string
      -- e.g. words "4 words and numbers" = ["4", "words", "and", "numbers"]
      -- The fmap function allows us to apply the function "words" to the string
      -- that the impure getLine function returns
      inputList <- fmap words getLine
      
      -- The number of all possible permutations of a list is equal to
      -- the factorial of the size the list
      let sizeOfPermutations = factorial (length inputList)
      
      -- The Data.List library that we imported has a permutation function that,
      -- given a list, returns a list containing all permutations of its argument
      let perms = permutations inputList
      
      -- We need to pick a random permutation, so we produce a random number.
      -- randomRIO stands for random "enumerable type" (i.e. ints, enums in other languages...)
      -- and uses IO operations to seed itself
      -- It generated a random element contained in the range specified in its argument
      -- Similarly to many other programming languages, in the Haskell stardard libraries,
      -- list are indexed starting at 0 
      randomPosition <- randomRIO (0, sizeOfPermutations - 1)
      
      -- The !! operator is used for indexing
      -- In many other languages, this line would read shuffledList = perms[randomPosition]
      let shuffledList = perms !! randomPosition
      
      -- Print can be used to output to the standard output
      -- When outputing a string, the string is surrounded by quotation marks
      -- When outputing a list, elements are separated by comma and the list is enclosed in square brackets
      -- This format is not the one specified for the problem though...
      
      print shuffledList
      
      -- ... so let's fix the format:
      
      -- The unwords function does the oposite of what the previously discussed words function does
      -- It takes a list of strings and return a string made up of the elements of the input
      -- list separated by one blank space (' ')
      let outputString = unwords shuffledList
      
      -- putStrLn (put string line) prints a string, without quotation marks,
      -- to standard output and inserts a new line (\n)
      putStrLn outputString