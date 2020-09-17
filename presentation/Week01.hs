{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week01 where

import Prelude hiding (Left, Right, Maybe (..), length, reverse, take, drop)

{-   CS316 FUNCTIONAL PROGRAMMING 2020/21

         Week 1 : DATA AND FUNCTIONS
-}

{-!      Part 1.1 : DEFINING DATATYPES AND VALUES

   This video introduces the two main concepts of functional
   programming in Haskell:

     1. Defining datatypes to represent problems.

     2. Transforming data values using pattern matching functions.
-}








{-! Examples of (built in) data values are:

     - Integers: 1, 2, 3, 4, -1, -100, ...
     - Floating point numbers: 1.1, 3.14, -0.65, ..
     - Strings: "hello", "Haskell", "rainbow"
     - Characters: 'a', 'b', 'x', 'y', 'z'
     - Booleans: True, False
-}










{-! Defining a Datatype -}

data Direction = Up | Down | Left | Right
  deriving Show













{-! Value definitions -}

whereIsTheCeiling :: Direction
whereIsTheCeiling = Up

whereIsTheFloor :: Direction
whereIsTheFloor = Down













{-! Defining functions -}

flipVertically :: Direction -> Direction
flipVertically Up    = Down
flipVertically Down  = Up
flipVertically Left  = Left
flipVertically Right = Right

isVertical :: Direction -> Bool
isVertical Up    = True
isVertical Down  = True
isVertical _     = False











{-! Defining Functions of two arguments -}

equalDirection :: Direction -> Direction -> Bool
equalDirection Up    Up    = True
equalDirection Down  Down  = True
equalDirection Left  Left  = True
equalDirection Right Right = True
equalDirection _     _     = False










{-! Summary:

     - Datatypes are defined by giving a list of constructor names
       separated by '|'s.

     - Functions are defined by saying what data to return for each
       possible input.
-}








{-!      Part 1.2 : CONSTRUCTORS WITH PARAMETERS -}

data DirectionWithDistance
  = UpD    Integer
  | DownD  Integer
  | LeftD  Integer
  | RightD Integer
  deriving Show







{-! Constructing Data with Parameters -}

withDistance :: Direction -> Integer -> DirectionWithDistance
withDistance Up    d = UpD d
withDistance Down  d = DownD d
withDistance Left  d = LeftD d
withDistance Right d = RightD d











{-! Pattern matching parameters -}

getDistance :: DirectionWithDistance -> Integer
getDistance (UpD d)    = d
getDistance (DownD d)  = d
getDistance (LeftD d)  = d
getDistance (RightD d) = d

getDirection :: DirectionWithDistance -> Direction
getDirection (UpD d)    = Up
getDirection (DownD d)  = Down
getDirection (LeftD d)  = Left
getDirection (RightD d) = Right








{-! Constructors with multiple parameters -}

data DirectionWithDistance2
  = DirectionWithDistance Direction Integer
  deriving Show

getDistance2 :: DirectionWithDistance2 -> Integer
getDistance2 (DirectionWithDistance dir dist) = dist

getDirection2 :: DirectionWithDistance2 -> Direction
getDirection2 (DirectionWithDistance dir dist) = dir





{-! Chaining Functions together -}

convertDirection :: DirectionWithDistance -> DirectionWithDistance2
convertDirection dwd = DirectionWithDistance (getDirection dwd) (getDistance dwd)

convertDirectionBack :: DirectionWithDistance2 -> DirectionWithDistance
convertDirectionBack (DirectionWithDistance dir dist) = withDistance dir dist










{-! Summary

    - Constructors of datatypes can have multiple parameters attached.

    - Constructing data with parameters: 'CName param1 param2 ...'

    - Pattern matching mirrors construction.
-}







{-!      Part 1.3 : DATATYPES AND FUNCTIONS WITH TYPE PARAMETERS -}

data Pair a b = MkPair a b
  deriving Show

type DirectionWithDistance3 = Pair Direction Integer









{-! Functions on Pairs -}

getFirst :: Pair a b -> a
getFirst (MkPair a b) = a

getSecond :: Pair a b -> b
getSecond (MkPair a b) = b

swap :: Pair a b -> Pair b a
swap (MkPair a b) = MkPair b a





{-! Maybe -}

data Maybe a
  = Nothing
  | Just a
  deriving Show

verticalDistance :: Pair Direction Integer -> Maybe Integer
verticalDistance (MkPair Up dist)    = Just dist
verticalDistance (MkPair Down dist)  = Just dist
verticalDistance (MkPair Left dist)  = Nothing
verticalDistance (MkPair Right dist) = Nothing






{-! Summary

  - Datatypes can be defined with type parameters, so they can work
    with multiple types.

  - Functions that work on these datatypes work for all possible
    types.

  - Type parameterised datatypes are a useful way to define common
    patterns of data types.
-}




{-!      Part 1.4 : RECURSIVE DATATYPES AND FUNCTIONS -}

data List a
  = Nil
  | Cons a (List a)
  deriving Show

onetwothree :: List Integer
onetwothree = Cons 1 (Cons 2 (Cons 3 Nil))

isNil :: List a -> Bool
isNil Nil        = True
isNil (Cons _ _) = False







{-! Making lists -}

singleton :: a -> List a
singleton a = Cons a Nil

listPair :: a -> a -> List a
listPair a1 a2 = Cons a1 (Cons a2 Nil)











{-! Built-in List syntax -}

singleton2 :: a -> [a]
singleton2 a = [a]

listPair2 :: a -> a -> [a]
listPair2 a b = [a,b]














{-! Head and Tail -}

getHead :: [a] -> Maybe a
getHead []     = Nothing
getHead (x:xs) = Just x

getTail :: [a] -> Maybe [a]
getTail []     = Nothing
getTail (x:xs) = Just xs








{-! Length of a list -}

length :: [a] -> Integer
length []     = 0
length (x:xs) = 1 + length xs











{-! Summing up a list -}

sumList :: [Integer] -> Integer
sumList []     = 0
sumList (x:xs) = x + sumList xs

--   sumList [1,2,3]
-- = sumList (1:2:3:[])
-- = 1 + sumList (2:3:[])
-- = 1 + 2 + sumList (3:[])
-- = 1 + 2 + 3 + sumList []
-- = 1 + 2 + 3 + 0
-- = 6









{-! Summary

    - Lists allow arbitrary size data to be represented.

    - Built in syntax for lists uses square brackets and commas.

    - Pattern matching on lists often requires recursive functions.
-}







{-!      Part 1.5 : APPENDING AND REVERSING LISTS -}

{- Append -}

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

--   append [1,2] [3,4]
-- = 1 : append [2] [3,4]
-- = 1 : 2 : append [] [3,4]
-- = 1 : 2 : [3,4]
-- = [1,2,3,4]







{-! Reverse -}

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs `append` [x]

--   reverse [1,2,3]
-- = append (reverse [2,3]) [1]
-- = append (append (reverse [3]) [2]) [1]
-- = append (append (append (reverse []) [3]) [2]) [1]
-- = append (append (append [] [3]) [2]) [1]
-- = [3,2,1]











{-! Fast Reverse -}

fastReverse :: [a] -> [a]
fastReverse xs = fastReverseHelper xs []

fastReverseHelper :: [a] -> [a] -> [a]
fastReverseHelper []     accum = accum
fastReverseHelper (x:xs) accum = fastReverseHelper xs (x:accum)

--   fastReverseHelper [1,2] []
-- = fastReverseHelper [2] (1:[])
-- = fastReverseHelper [] (2:1:[])
-- = [2,1]









{-! Summary


    - Two useful functions on lists are 'append' and 'reverse'.

    - They are both defined by recursion over the input list.

    - Fast reverse reverses a list by using an accumulator, giving
      linear time complexity.
-}






{-!      Part 1.6 : TAKE, DROP AND CHUNK -}

{- Take -}

take :: Integer -> [a] -> [a]
take 0 xs     = []
take n []     = []
take n (x:xs) = x : take (n-1) xs









{-! Drop -}

drop :: Integer -> [a] -> [a]
drop 0 xs     = xs
drop n []     = []
drop n (x:xs) = drop (n-1) xs











{-! Chunk -}

chunk :: Integer -> [a] -> [[a]]
chunk n [] = []
chunk n xs = take n xs : chunk n (drop n xs)

-- chunk 0 [1,2,3,4,5]
-- take 0 [1,2,3,4,5] : chunk 0 (drop 0 [1,2,3,4,5])
-- [] : chunk 0 [1,2,3,4,5]
-- [] : [] : [] : [] : ...











{-! Summary

   - Functions on lists can be defined by recursion on numbers and not
     the list itself.

   - 'take' and 'drop' are examples of such.

   - Lists can be nested.
-}
