{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week1 where

import Prelude hiding (take, drop, Left, Right, Maybe (..), reverse,length)

{- CS316 2020/21 : Week 1

       DATA AND FUNCTIONS

   Pre-requisites: GETTING STARTED WITH HASKELL
-}


{-     PART 1 : DEFINING DATA AND VALUES

   This first week introduces the two main concepts in Haskell:

     1. Defining the data we use to represent problems.

     2. Transforming data by pattern matching, to solve problems.


   Examples of (built in) data values are:

     - Strings: "hello", "Haskell", "rainbow"
     - Characters: 'a', 'b', 'x', 'y', 'z'
     - Booleans: True, False

   Every data value is also a (short) Haskell program that computes
   itself as its result.

   Let's try that in GHCi, the interactive "Read-Eval-Print-Loop"
   (REPL) for Haskell...

     FIXME

   Data values in Haskell are arranged into data types. We can ask
   GHCi what the types of values are by using the ':t' command:

     FIXME

   One of the most useful aspects of Haskell is the ability to define
   our own data types.

   It is often better to define our own data types for the problem we
   want to solve, so that we can be more precise in our modelling.

   Let's say we want to write programs that manipulate the directions
   up, down, left, and right. We can define a data type in Haskell to
   do this by the following declaration: -}

data Direction = Up | Down | Left | Right
  deriving Show

{- In English, we read this declaration as:

   A 'Direction' is either
   - 'Up'; or
   - 'Down'; or
   - 'Left'; or
   - 'Right'

   The symbol '|' is read as 'or'. Another way to read this
   declaration is: "a Direction is either Up, Down, Left, or Right".

   Terminology:

   - 'Direction' is the name of the data type. Data type names always
     start with a capital letter (apart from some special built-in
     cases).

   - 'Up', 'Down', 'Left', and 'Right' are names of *constructors* of
     the values of the data type 'Direction'. Constructors always
     start with capital letters (again, apart from some special
     built-in cases).

     NOTE: the 'deriving Show' is an instruction to the Haskell
     compiler to generate a function for converting 'Direction's to
     strings. Think of this as auto-generating something similar to
     Java's 'toString()' methods.
-}

{- Now that have defined a datatype, we can make some value
   definitions. Here is a value definition: -}

whereIsTheCeiling :: Direction
whereIsTheCeiling = Up

{- This value definition consists of two lines:

   1. The first line tells us the name of the value we are defining
      ('whereIsTheCeiling') and what type it is ('Direction').

   2. The second line repeats the name and gives the actual
      definition, after an '='. In this case, we are defining
      'whereIsTheCeiling' to be 'Up'.

   We can now use the name 'whereIsTheCeiling' to stand for 'Up'.

   The first line telling Haskell what the type is often optional. The
   Haskell compiler is able to deduce the type from the value
   itself. So we can make another value definition 'whereIsTheFloor'
   by just giving the definition itself: -}

whereIsTheFloor = Down

{- It is good practice to always give the types though. It makes the
   code much more readable, and improves the error messages that you
   get back from the compiler. -}

{- Making definitions like this is not very useful by itself. All we can
   do is give names to existing pieces of data. More usefully, we can
   define /functions/ that transform data.

   Roughly speaking, a function in Haskell takes in some data, looks
   at it to decide what to do, and then returns some new data.

   Here is an example that transforms directions by flipping them
   vertically: -}

flipVertically :: Direction -> Direction
flipVertically Up    = Down
flipVertically Down  = Up
flipVertically Left  = Left
flipVertically Right = Right

{- This definition looks more complex, but is similar to the previous
   ones. The first line tells us (and the Haskell compiler) what type
   'flipVertically' has. In this case it is a /function/ type
   'Direction -> Direction', meaning that it is a function that takes
   'Direction's as input, and returns 'Direction's as output.

   The other four lines define what happens for each of the four
   different 'Direction's: the two vertical directions are flipped,
   and the two horizontal directions remain the same. This style of
   writing functions is called "pattern matching" -- each line of the
   definition defines a pattern of input that it recognises, and
   defines what to do with it on the right hand side of the equals
   sign.

   Functions need not only convert data into data of the same
   type. For example, we can write a function by pattern matching that
   returns 'True' if the input is a vertical direction, and 'False' if
   is a horizontal direction: -}

isVertical :: Direction -> Bool
isVertical Up    = True
isVertical Down  = True
isVertical Left  = False
isVertical Right = False

{- We can also match on more than one input at a time. Here is a
   function that takes two 'Direction's as input and returns 'True' if
   they are the same, and 'False' otherwise. This definition also
   introduces a new kind of pattern: "wildcard" patterns, written
   using an underscore '_'. These patterns stand for any input that
   wasn't matched by the previous patterns. -}

equalDirection :: Direction -> Direction -> Bool
equalDirection Up    Up    = True
equalDirection Down  Down  = True
equalDirection Left  Left  = True
equalDirection Right Right = True
equalDirection _     _     = False

{- The type of 'equalDirection' is 'Direction -> Direction -> Bool',
   indicating that it takes two 'Direction's as input, and returns a
   'Bool'ean. The next four lines define the 'True' cases: when the
   two input 'Direction's are the same. The final line says that
   "whenever none of the previous cases applies, return 'False'". Note
   that patterns are tested in order, from top to bottom. -}

{- Questions:

   1. Multiple choice: in the declaration 'Direction = ...'
   'Direction' is (a) a datatype name; (b) a name of a defined value;
   (c) a constructor. Similar for other ones.

   1. Define flipHorizontally

   2. Define isHorizontal

   3. Define isClockwiseOf
-}

flipHorizontally :: Direction -> Direction
flipHorizontally Up = Up
flipHorizontally Down = Down
flipHorizontally Left = Right
flipHorizontally Right = Left

isHorizontal :: Direction -> Bool
isHorizontal Left  = True
isHorizontal Right = True
isHorizontal Up    = False
isHorizontal Down  = False

isClockwiseOf :: Direction -> Direction -> Bool
isClockwiseOf Up    Right = True
isClockwiseOf Right Down  = True
isClockwiseOf Down  Left  = True
isClockwiseOf Left  Up    = True
isClockwiseOf _     _     = False

{-     PART 2 : STRUCTURED DATA


-}

data DirectionWithDistance
  = UpD    Int
  | DownD  Int
  | LeftD  Int
  | RightD Int
  deriving Show



data PairOfDirections
  = PairOfDirections Direction Direction
  deriving Show

isPairOfEqualDirections :: PairOfDirections -> Bool
isPairOfEqualDirections (PairOfDirections Up    Up)    = True
isPairOfEqualDirections (PairOfDirections Down  Down)  = True
isPairOfEqualDirections (PairOfDirections Left  Left)  = True
isPairOfEqualDirections (PairOfDirections Right Right) = True
isPairOfEqualDirections _                        = False

-- demonstrating

swapDirections :: PairOfDirections -> PairOfDirections
swapDirections (PairOfDirections d1 d2) = PairOfDirections d2 d1

getFirstDirection :: PairOfDirections -> Direction
getFirstDirection (PairOfDirections d1 _) = d1

getSecondDirection :: PairOfDirections -> Direction
getSecondDirection (PairOfDirections _ d2) = d2

data Pair a b = Pair a b

first :: Pair a b -> a
first (Pair a b) = a

second :: Pair a b -> b
second (Pair a b) = b

directionToOffset :: Direction -> Pair Int Int
directionToOffset Up    = Pair 1    0
directionToOffset Down  = Pair (-1) 0
directionToOffset Left  = Pair 0    (-1)
directionToOffset Right = Pair 0    1

data Maybe a
  = Nothing
  | Just a
  deriving Show

{- Questions:

   1. Rewrite 'isPairOfEqualDirections' using the 'Pair' datatype
      (give them the type signature).

   2. Rewrite swap for Pair.

        swap :: Pair a b -> Pair b a
        swap (Pair a b) = Pair b a

   3.

-}

{-     PART 3 : LISTS -}

data List a
  = Nil
  | Cons a (List a)
  deriving Show

-- Examples, and how they are type checked


-- Official syntax

length :: [a] -> Int
length []       = 0
length (x : xs) = 1 + length xs



{- Questions:

   1. Write 'sum' function

   2. Write 'average', using 'sum' and 'length'
-}

{-     PART 4 : LIST MANIPULATION -}

take :: Int -> [a] -> [a]
take 0 xs       = []
take n (x : xs) = x : take (n-1) xs
take n []       = []

drop :: Int -> [a] -> [a]
drop 0 xs       = xs
drop n (x : xs) = drop (n-1) xs
drop n []       = []

{- Questions:

   1. Write a function 'dropSpaces' :: [Char] -> [Char] that drops
   spaces from the start of a list of characters.
-}

{-     PART 5 : APPENDING AND REVERSING -}

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

reverse :: [a] -> [a]
reverse []     = []
reverse (x:xs) = reverse xs `append` [x]

{- Questions

   1. Using 'reverse' and 'dropSpaces', write a function that removes
   spaces at the *end* of a list of characters.

   2. Write 'revAppend'.
-}

{-     PART 6 : FUNCTIONS THAT USE OTHER FUNCTIONS -}

chunk :: Int -> [a] -> [[a]]
chunk n [] = []
chunk n xs = take n xs : chunk n (drop n xs)

{- Question:

   1. Alter chunk so that the chunks get one larger each time.
-}

removePrefix :: Eq a => [a] -> [a] -> Maybe [a]
removePrefix []     ys     = Just ys
removePrefix (x:xs) []     = Nothing
removePrefix (x:xs) (y:ys) = if x == y then removePrefix xs ys else Nothing

reverseMaybe :: Maybe [a] -> Maybe [a]
reverseMaybe Nothing   = Nothing
reverseMaybe (Just xs) = Just (reverse xs)

{- Question:

   1. use 'removePrefix' to write a function that removesSuffixes.
          (provide hints)
-}
