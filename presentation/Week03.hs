{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week03 where

import Prelude hiding ((.), map, filter)

{-!    WEEK 3 : HIGHER ORDER FUNCTIONS

   This week we will look at the concept of "functions as values".
-}











{-!     Part 3.1 : FUNCTIONS THAT RETURN FUNCTIONS -}

add :: Int -> (Int -> Int)
add x y = x + y

addTen :: Int -> Int
addTen = add 10

{-   addTen 5
   = add 10 5
   = 10 + 5
   = 15
-}

addTen' :: Int -> Int
addTen' y = add 10 y

add' :: Int -> (Int -> Int)
add' = \x -> (\y -> x + y)

-- \x -> <expression>
--
--  "a function that takes an 'x' and returns <expression>"

add'' :: Int -> Int -> Int
add'' = \x y -> x + y




{-! Partial application -}

prefixWith :: String -> String -> String
prefixWith prefix str = prefix ++ ": " ++ str

errorMessage :: String -> String
errorMessage = prefixWith "ERROR"

logMessage :: String -> String
logMessage = prefixWith "LOG"













{-! Lambda notation -}

-- \ x -> x + x













{-! Summary

  - Functions are first class values in Haskell

  - Functions of multiple arguments work by returning functions

  - Lambda notation is a way of writing functions anonymously
-}







{-!     Part 3.2 : FUNCTIONS THAT TAKE OTHER FUNCTIONS AS INPUTS

  Generalisation steps, from ten to octtuple -}

ten :: Int
ten = add 5 5

double :: Int -> Int
double x = add x x

ten' :: Int
ten' = double 5

applyCopy :: (Int -> Int -> Int) -> Int -> Int
applyCopy f x = f x x

double' :: Int -> Int
double' = applyCopy add



quadruple :: Int -> Int
quadruple x = double (double x)

twice :: (a -> a) -> a -> a
twice f x = f (f x)

quadruple2 :: Int -> Int
quadruple2 = twice double

{-  quadruple2 x
  = twice double x
  = double (double x)
  = quadruple x
-}

octtuple :: Int -> Int
octtuple = twice quadruple
    --   = twice (twice double)



{-! Summary

  - Functions can take other functions as input

  - This allows functions that capture generic patterns

  - We can get from specific to general functions

  - We specialise generic functions by supplying functions as
    parameters

  - Lambda notation is useful for this, so we don't have to name
    everything.

-}

{-!     Part 3.3 : MAP AND FILTER -}

doubleAll0 :: [Int] -> [Int]
doubleAll0 []     = []
doubleAll0 (x:xs) = (x*2) : doubleAll0 xs

map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs

--   map double [1,2,3,4,5]
-- = double 1 : map double [2,3,4,5]
-- = double 1 : double 2 : map double [3,4,5]
-- = ..
-- = 2 : 4 : ..

doubleAll :: [Int] -> [Int]
doubleAll = map double








{-! Applications of 'map'

  'map' "lifts" a function.
-}

firsts :: [(a,b)] -> [a]
firsts = map fst

withLengths :: [String] -> [(String,Int)]
withLengths = map (\s -> (s, length s))





{-! Filtering -}

onlyEvens0 :: [Int] -> [Int]
onlyEvens0 []     = []
onlyEvens0 (x:xs) = if x `mod` 2 == 0 then x : onlyEvens0 xs else onlyEvens0 xs

filter :: (a -> Bool) -> [a] -> [a]
filter p []     = []
filter p (x:xs) = if p x then x : filter p xs else filter p xs

onlyEvens :: [Int] -> [Int]
onlyEvens = filter isEven
  where isEven x = x `mod` 2 == 0









{-! Rewriting Quicksort -}

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
   where smaller = filter (\y -> y < x) xs     -- [ y | y <- xs, y < x ]
         larger  = filter (\y -> y >= x) xs    -- [ y | y <- xs, y >= x ]








{-! Summary

  - 'map' is a generic function for "lifting" a function on items to a
    function on lists of items.

  - 'filter' is a generic function for only keeping some items in a
    list.

  - Both a customisable patterns of recursion.
-}





{-!     Part 3.4 : COMPOSITION AND IDENTITY -}














{-! Composition -}

dropSpaces :: String -> String
dropSpaces []       = []
dropSpaces (' ':xs) = dropSpaces xs
dropSpaces xs       = xs

dropTrailingSpaces :: String -> String
dropTrailingSpaces xs = reverse (dropSpaces (reverse xs))

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

dropTrailingSpaces1 :: String -> String
dropTrailingSpaces1 = reverse . dropSpaces . reverse





{-! Making pipelines -}

{- grep CS316 | cut -f1 -d',' < registered-students.csv -}

findCS316Students :: [(String,String)] -> [String]
findCS316Students = map (\(regno,course) -> regno) . filter (\(regno,course) -> course == "CS316")

findCS316Students2 :: [(String,String)] -> [String]
findCS316Students2 = map getRegNo . filter takingCS316
  where takingCS316 (regno,course) = course == "CS316"
        getRegNo (regno,course) = regno

{- grep CS316 | wc -l < registered-students.csv -}

numberOfCS316Students :: [(String,String)] -> Int
numberOfCS316Students = length . filter (\(regno,course) -> course == "CS316")








{-! The Identity Function -}

identity :: a -> a
identity x = x

findCS316Students3 :: [(String,String)] -> [(String,String)]
findCS316Students3 = filter (\(regno,course) -> course == "CS316")













{-! Summary

    - Composition (.) is a useful function that connects two functions
      together.

    - It can be used to make "pipelines" for processing lots of data

    - The identity function is the pipeline that does nothing.
-}







{-!     Part 3.5 : MAP FOR OTHER DATATYPES -}

-- map :: (a -> b) -> [a] -> [b]

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf         = Leaf
mapTree f (Node l a r) = Node (mapTree f l) (f a) (mapTree f r)






{-! General form of map -}

-- mapX :: (a -> b) -> X a -> X b












{-! 'map' for Maybe -}

{- data Maybe
     = Nothing
     | Just a
-}

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing = Nothing
mapMaybe f (Just a) = Just (f a)










{-! 'map' for WithString -}

data WithString a
  = MkWithString (String -> a)

lengths :: WithString Int
lengths = MkWithString (\s -> length s)

valueOf :: String -> WithString a -> a
valueOf s (MkWithString f) = f s

mapWithString :: (a -> b) -> WithString a -> WithString b
mapWithString f (MkWithString g) = MkWithString (f . g)
   -- f :: a -> b
   -- g :: String -> a








{-! Can we always define 'map'? -}

data Fun a = MkFun (a -> a)

mapFun :: (a -> b) -> Fun a -> Fun b
mapFun f (MkFun g) = MkFun id
  -- f :: a -> b
  -- g :: a -> a
  -- ?? :: b -> b







{-! What makes a good 'map'? -}

{- 1. Preservation of identity -}

--  mapX id x == x

-- mapMaybe id Nothing  == Nothing
-- mapMaybe id (Just x) == Just (id x) == Just x

-- mapFun id (MkFun g) = MkFun id

{- 2. Preservation of compose -}

-- mapX f (mapX g x) == mapX (f . g) x









{-! Summary

  - We can define 'map' for any kind of "container" like datatype.

  - Examples:
    - Lists
    - Trees
    - Maybe
    - Functions 'X -> a'

  - Non-examples:
    - Self functions 'a -> a'

  - Functor laws
-}
