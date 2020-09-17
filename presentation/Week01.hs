{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week01 where

import Prelude hiding (Left, Right, Maybe (..))

{-   CS316 FUNCTIONAL PROGRAMMING 2020/21

         Week 1 : DATA AND FUNCTIONS
-}

{-!      Part 1.1 : DEFINING DATA TYPES AND VALUES

   This first week introduces the two main concepts in Haskell:

     1. Defining the data types to represent problems.

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
whereIsTheCeiling = undefined












{-! Defining functions -}

flipVertically :: Direction -> Direction
flipVertically = undefined

isVertical :: Direction -> Bool
isVertical = undefined











{-! Defining Functions of two arguments -}

equalDirection :: Direction -> Direction -> Bool
equalDirection = undefined











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
withDistance = undefined











{-! Pattern matching parameters -}

getDistance :: DirectionWithDistance -> Integer
getDistance = undefined

getDirection :: DirectionWithDistance -> Direction
getDirection = undefined








{-! Constructors with multiple parameters -}

data DirectionWithDistance2
  = DirectionWithDistance Direction Integer
  deriving Show

getDistance2 :: DirectionWithDistance2 -> Integer
getDistance2 = undefined

getDirection2 :: DirectionWithDistance2 -> Direction
getDirection2 = undefined





{-! Chaining Functions together -}

convertDirection :: DirectionWithDistance -> DirectionWithDistance2
convertDirection = undefined











{-! Summary

    - Constructors of datatypes can have multiple parameters attached.

    - Constructing data with parameters: 'CName param1 param2 ...'

    - Pattern matching mirrors construction.
-}







{-!      Part 1.3 : DATATYPES AND FUNCTIONS WITH PARAMETERS -}

data Pair a b = MkPair a b
  deriving Show

type DirectionWithDistance3 = Pair Direction Integer









{- Functions on Pairs -}

getFirst :: Pair a b -> a
getFirst = undefined

getSecond :: Pair a b -> b
getSecond = undefined

swap :: Pair a b -> Pair b a
swap = undefined

{- Maybe -}

data Maybe a
  = Nothing
  | Just a
  deriving Show

verticalDistance :: Pair Direction Integer -> Maybe Integer
verticalDistance (MkPair Up   dist) = Just dist
verticalDistance (MkPair Down dist) = Just dist
verticalDistance (MkPair _    dist) = Nothing





{-!      Part 1.4 : RECURSIVE DATATYPES AND FUNCTIONS -}

data List a
  = Nil
  | Cons a (List a)
  deriving Show









{-! Making lists -}














{-! Built-in List syntax -}














{-! Head and Tail -}

getHead :: [a] -> Maybe a
getHead = undefined

getTail :: [a] -> Maybe [a]
getTail = undefined








{-! Length of a list -}

length :: [a] -> Integer
length = undefined











{-! Summing up a list -}

sumList :: [Integer] -> Integer
sumList = undefined


{-! Summary

    - Lists allow arbitrary size data to be represented.

    - Built in syntax for lists uses square brackets and commas.

    - Pattern matching on lists often requires recursive functions.
-}







{-!      Part 1.5 : APPENDING AND REVERSING LISTS -}

{- Append -}

append :: [a] -> [a] -> [a]
append = undefined









{-! Reverse -}

reverse :: [a] -> [a]
reverse = undefined











{-! Fast Reverse -}

fastReverse :: [a] -> [a]
fastReverse = undefined

fastReverseHelper :: [a] -> [a] -> [a]
fastReverseHelper = undefined







{-! Summary


    - Two useful functions on lists are 'append' and 'reverse'.

    - They are both defined by recursion over the input list.

    - Fast reverse reverses a list by using an accumulator.
-}






{-!      Part 1.6 : TAKE, DROP AND CHUNK -}

{- Take -}

take :: Integer -> [a] -> [a]
take = undefined









{-! Drop -}

drop :: Integer -> [a] -> [a]
drop = undefined











{-! Chunk -}

chunk :: Integer -> [a] -> [[a]]
chunk = undefined
