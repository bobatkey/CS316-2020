{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week01 where

import Prelude hiding (take, drop, Left, Right, Maybe (..), reverse, length)

{- CS316 2020/21 : Week 1

             DATA AND FUNCTIONS

   We start the course with an introduction to the two fundamental
   concepts of functional programming in Haskell:

     1. Defining the datatypes we use to represent problems.

     2. Transforming data by pattern matching functions.

   Almost all of what you will learn in this course is an elaboration
   of these two key points. Over the weeks of this course, we will see
   many different ways of representing problems using datatypes, and
   several different ways to design functions to transform data. -}


{-     Part 1.1 : DEFINING DATATYPES AND FUNCTIONS

   Examples of (built in) data values in Haskell are:

     - Integers: 1, 2, 3, 100, -10
     - Strings: "hello", "Haskell", "rainbow"
     - Characters: 'a', 'b', 'x', 'y', 'z'
     - Booleans: True, False

   Every data value is also a (short) Haskell program that computes
   itself as its result. Which means that if we give Haskell the
   program '1', it will evaluate to '1'.

   Let's try that in GHCi, the interactive "Read-Eval-Print-Loop"
   (REPL) for Haskell...

     *Week01> 1
     1
     *Week01> 2
     2
     *Week01> "hello"
     "hello"
     *Week01> True
     True

   Data values in Haskell are classified by their types. We can ask
   GHCi what the types of values are by using the ':t' command:

     *Week01> :t True
     True :: Bool

   This notation says "True" "has type" "Bool". The double colon is
   read as "has type".

   The built in datatypes are useful, but one of the most useful
   aspects of Haskell is the ability to define new datatypes.

   It is often better to define our own datatypes for the problem we
   want to solve, rather than using existing types. This is so that we
   can be more precise in our modelling of the problem. The design of
   our datatypes drives the design of our programs, helping us write
   the programs too.

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

   The keyword 'data' means that this is a datatype. The symbol '|' is
   read as 'or'. Another way to read this declaration is: "a Direction
   is either Up, Down, Left, or Right".

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
     strings, so they can be printed out. Think of this as
     auto-generating something similar to Java's 'toString()' methods.

   Now that we have defined a datatype, we can make some value
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

   The first line specifying what the type is often optional. The
   Haskell compiler is able to deduce the type from the value
   itself. So we can make another value definition 'whereIsTheFloor'
   by just giving the definition itself: -}

whereIsTheFloor = Down

{- (NOTE: if you are reading this in VSCode with the Haskell integration
   turned on, it will insert the inferred type just above the
   definition.)

   Even though we can often omit types, it is good practice to always
   give them for definitions we make. It makes code much more
   readable, and improves the error messages that you get back from
   the compiler. -}

{- Making definitions like this is not very useful by itself. All we can
   do is give names to existing values. More usefully, we can define
   functions that transform data.

   Roughly speaking, a function in Haskell takes in some data, looks
   at it to decide what to do, and then returns some new data.

   Here is an example that transforms directions by flipping them
   vertically: -}

flipVertically :: Direction -> Direction
flipVertically Up    = Down
flipVertically Down  = Up
flipVertically Left  = Left
flipVertically Right = Right

{- This definition looks more complex, but is similar to the two above.

   The first line tells us (and the Haskell compiler) what type
   'flipVertically' has. It is a function type 'Direction ->
   Direction', meaning that it is a function that takes 'Direction's
   as input, and returns 'Direction's as output.

   The other four lines define what happens for each of the four
   different 'Direction's: the two vertical directions are flipped,
   and the two horizontal directions remain the same.

   This style of writing functions by enumerating possible cases is
   called "pattern matching" -- each line of the definition specifies
   a pattern of input that it recognises, and defines what to do on
   the right hand side of the equals sign.

   Functions need not only translate values into values of the same
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
   they are the same, and 'False' otherwise.

   This definition also introduces a new kind of pattern: "wildcard"
   patterns, written using an underscore '_'. These patterns stand for
   any input that wasn't matched by the previous patterns. -}

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
   that patterns are matched in order, from top to bottom. -}

{-     Part 1.2 : STRUCTURED DATA

   We have seen how to define a datatype that consist of several
   options: a Direction can be any of Up, Down, Left, or Right. But
   what if we want to represent how far up, down, left or right?

   To solve problems like this, datatypes can also have additional
   data attached to each constructor. We do this by writing the types
   of the data we want to attach after the constructor name in the
   'data' declaration.

   Here is an example that defines a datatype called
   'DirectionWithDistance'. It has the same number of constructors as
   'Direction', but now they all have an 'Integer' attached. -}

data DirectionWithDistance
  = UpD    Integer
  | DownD  Integer
  | LeftD  Integer
  | RightD Integer
  deriving Show

{- The effect of this declaration is to define a new datatype
   'DirectionWithDistance' with four constructors 'UpD', 'DownD',
   'LeftD' and 'RightD'. The difference with the previous constructors
   that these now take parameters:

      *Week01> UpD 100
      UpD 100
      *Week01> LeftD 300
      LeftD 300

   If we try to type 'UpD' by itself, we get an error message:

      *Week01> UpD

      <interactive>:6:1: error:
          • No instance for (Show (Integer -> DirectionWithDistance))
              arising from a use of ‘print’
              (maybe you haven't applied a function to enough arguments?)
          • In a stmt of an interactive GHCi command: print it

   This error message is saying that the thing we typed in ('UpD') has
   type 'Integer -> DirectionWithDistance', and that this type has no
   'Show' instance. We will cover what it means to have a 'Show'
   instance later in the course, but in essence it means that Haskell
   doesn't know how to print out functions.

   This error message has revealed that 'UpD' is in fact a
   function. We can see this by asking the type:

      *Week01> :t UpD
      UpD :: Integer -> DirectionWithDistance

   Writing an integer after 'UpD' yields a complete
   'DirectionWithDistance':

      *Week01> :t UpD 100
      UpD 100 :: DirectionWithDistance

   Now that we know how to construct values of
   'DirectionWithDistance', we can write a function to construct them
   for us.

   The following function takes a 'Direction' and an 'Integer' and
   puts them together to create a 'DirectionWithDistance': -}

withDistance :: Direction -> Integer -> DirectionWithDistance
withDistance Up    d = UpD d
withDistance Down  d = DownD d
withDistance Left  d = LeftD d
withDistance Right d = RightD d

{- This definition introduces a feature we haven't seen yet: using
   variables in patterns to stand for any value.

   In each line the 'd' before the '=' sign is a pattern that stands
   for any 'Integer' that is passed to the function. So if we call it
   like so:

      *Week01> withDistance Up 100

   then the first line is used, with 'd' set to '100'. And we get the
   response:

      UpD 100

   In general, we can put variables anyway in patterns on the left
   hand side of the equals where we would put a concrete value, and it
   will be set to whatever matches at that point in the input.

   We can see variables in action when we try to match against values
   of type 'DirectionWithDistance'. The following function takes
   'DirectionWithDistance' values as input, and extracts the 'Integer'
   part: -}

getDistance :: DirectionWithDistance -> Integer
getDistance (UpD d)    = d
getDistance (DownD d)  = d
getDistance (LeftD d)  = d
getDistance (RightD d) = d

{- So if we call this function with 'RightD 300' as input, then the
   fourth line matches with 'd' set to '300':

     *Week01> getDistance (RightD 300)
     300

   Similarly, we can write a function that extracts the 'Direction'
   from a 'DirectionWithDistance': -}

getDirection :: DirectionWithDistance -> Direction
getDirection (UpD d)    = Up
getDirection (DownD d)  = Down
getDirection (LeftD d)  = Left
getDirection (RightD d) = Right

{- We are not limited to only one parameter on each constructor.

   The 'DirectionWithDistance' type can be thought of as representing
   pairs of 'Direction's and distances. We can make this explicit by
   defining a datatype with a single constructor with two parameters:
   the 'Direction' and the 'Integer' distance: -}

data DirectionWithDistance2
  = DirectionWithDistance Direction Integer
  deriving Show

{- (I have had to call this 'DirectionWithDistance2' because we cannot
   have two datatypes with the same name in the same module. Similarly
   for the 'getDirection' and 'getDistance' functions below.)

   With this datatype, it is much easier to write the 'getDistance'
   and 'getDirection' functions. We no longer have to write a case for
   each direction: -}

getDistance2 :: DirectionWithDistance2 -> Integer
getDistance2 (DirectionWithDistance dir dist) = dist

getDirection2 :: DirectionWithDistance2 -> Direction
getDirection2 (DirectionWithDistance dir dist) = dir

{- The 'DirectionWithDistance' and 'DirectionWithDistance2' datatypes
   both represent the same information, in different ways. We can
   demonstrate this relationship by writing functions that convert
   back and forth. This will give us our first examples of using
   functions within the definitions of other functions: -}

convertDirection :: DirectionWithDistance -> DirectionWithDistance2
convertDirection dwd = DirectionWithDistance (getDirection dwd) (getDistance dwd)

convertDirectionBack :: DirectionWithDistance2 -> DirectionWithDistance
convertDirectionBack (DirectionWithDistance dir dist) = withDistance dir dist

{- In the first function, we get a 'DirectionWithDistance' called
   'dwd'. Then we construct a new 'DirectionWithDistance2' by using
   its constructor. For the two parameters, we use 'getDirection' to
   extract the direction from 'dwd' and 'getDistance' to extract the
   distance.

   In the second function, we pattern match on the single constructor
   of 'DirectionWithDistance2', and then use the 'withDistance'
   function defined above to construct a 'DirectionWithDistance'. -}



{-!      Part 1.3 : DATATYPES AND FUNCTIONS WITH TYPE PARAMETERS


  We defined the datatype 'DirectionWithDistance2' to represent
  directions with distances as pairs, and defined two functions
  'getDirection' and 'getDistance' to extract the two parts.

  But generally speaking, having pairs of things seems like a useful
  thing to have for any pair of types, not just 'Direction's and
  'Integer's.

  To accomplish this, we can parameterise datatype definitions by
  types, to get a generic datatype that can be specialised to any
  particular type. Here is how we can define a generic pair type: -}

data Pair a b = MkPair a b
  deriving Show

{- The new thing here is the 'a' and 'b' between the name of the
   datatype and the 'equals'. This indicates that this datatype takes
   two type parameters called 'a' and 'b'. These are then used in the
   constructor 'MkPair' where we used concrete type names before.

   We can now use the 'MkPair' constructor to construct pairs of any
   types of values:

      *Week01> MkPair 1 2
      MkPair 1 2
      *Week01> MkPair Up Left
      MkPair Up Left
      *Week01> MkPair 'a' 'b'
      MkPair 'a' 'b'
      *Week01> MkPair Up 'a'
      MkPair Up 'a'

   and we can ask what their types are, showing us that have different
   'Pair's of different types of values:

      *Week01> :t MkPair Up Left
      MkPair Up Left :: Pair Direction Direction
      *Week01> :t MkPair Up 'a'
      MkPair Up 'a' :: Pair Direction Char

   Using 'Pair' we can build another way of representing pairs of
   'Direction's and distances: -}

type DirectionWithDistance3 = Pair Direction Integer

{- The keyword 'type' means that we are defining a "type synonym", that
   is, an alternative name for a type. This definition says that we
   will use the name 'DirectionWithDistance3' to stand for the type
   'Pair Direction Integer'. -}

{- To define functions that manipulate 'Pair's, we need to write types
   with parameters as well. We do this by writing them with type
   variables 'a', 'b', etc. where there would be concrete types. The
   following two functions have similar definitions to 'getDistance2'
   and 'getDirection2' above, but work for any types 'a' and 'b': -}

getFirst :: Pair a b -> a
getFirst (MkPair a b) = a

getSecond :: Pair a b -> b
getSecond (MkPair a b) = b

{- Since 'Pair's can be constructed for any types 'a' and 'b', we can
   write functions that move them around. For example, swapping the
   components of a pair: -}

swap :: Pair a b -> Pair b a
swap (MkPair a b) = MkPair b a


{- We have seen several features of datatypes now: multiple
 constructors, value parameters, and type parameters. The following
 extremely useful type mixes the all together in one use: -}

data Maybe a
  = Nothing
  | Just a
  deriving Show

{- The 'Maybe' type is useful for representing values that may or may
   not be present. If a value is present, then we can write 'Just 1'
   (for example). If there is no sensible value to give, then we write
   'Nothing'. The 'Maybe' type is similar to the use of 'null' in a
   language like Java, except that the possible absence of a value is
   made explicit in the type.

   Here is an example of the use of 'Maybe'. The function
   'verticalDistance' is expected to return the vertical distance
   associated with a pair of a 'Direction' and a distance. In the
   cases when the direction is 'Left' or 'Right', however, there is
   nothing sensible to return (the direction is horizontal). So we
   return 'Nothing': -}

verticalDistance :: Pair Direction Integer -> Maybe Integer
verticalDistance (MkPair Up dist)    = Just dist
verticalDistance (MkPair Down dist)  = Just dist
verticalDistance (MkPair Left dist)  = Nothing
verticalDistance (MkPair Right dist) = Nothing

{- We will see the 'Maybe' type many times in this course. -}



{-!      Part 1.4 : RECURSIVE DATATYPES AND FUNCTIONS


  The datatypes we have seen so far only allow us to represent fixed
  amounts of data. However, most programs we want to write will use
  arbitrarily sized amounts of data.

  To do this in Haskell, we use datatypes that refer to themselves
  recursively. Here is an example, used for representing lists: -}

data List a
  = Nil
  | Cons a (List a)
  deriving Show

{- The 'List' datatype takes a type parameter 'a' standing for the type
   of data stored in the list.

   There are two constructors:

   1. 'Nil' represents the empty list. It does not have any parameters.

   2. 'Cons' represents an element followed by more list.

   To represent lists, we start from 'Nil', and build up the list by
   adding things to the front: -}

empty :: List Integer
empty = Nil

three :: List Integer
three = Cons 3 empty

twothree :: List Integer
twothree = Cons 2 three

onetwothree :: List Integer
onetwothree = Cons 1 twothree

{- Now if we evaluate 'onetwothree', we can see the whole list laid out:

      *Week01> onetwothree
      Cons 1 (Cons 2 (Cons 3 Nil))

   Pattern matching on lists works as before: -}

isNil :: List a -> Bool
isNil Nil        = True
isNil (Cons _ _) = False

{- We can construct lists in functions by using the constructors: -}

singleton :: a -> List a
singleton a = Cons a Nil

listPair :: a -> a -> List a
listPair a1 a2 = Cons a1 (Cons a2 Nil)

{- Lists are so useful in Haskell (possibly they are *over*used) that
   they have a special built in syntax.

   The list type is written '[a]' to stand for "a list of elements of
   type 'a'".

   The empty list 'Nil' is written '[]'.

   To add an element to the head of a list, we write '1 : []', instead
   of using 'Cons'. Note that ':' is still pronouced 'Cons' (short for
   'Construct').

   To write out full lists, we can use square brackets and commas,
   like so:

      [1,2,3,4]

   is shorthand for

      1 : 2 : 3 : 4 : []

   When Haskell prints out lists it always uses the brackets and
   commas form. However, it is important to remember that lists are
   really formed from the constructors ':' and '[]'.

   We can rewrite the two list construction functions above in terms
   of the built-in syntax: -}

singleton2 :: a -> [a]
singleton2 a = [a]

listPair2 :: a -> a -> [a]
listPair2 a b = [a,b]

{- Pattern matching on lists uses the ':' and '[]' syntax too. These two
   functions extract the head or tail of a list, provided it exists. I
   have used the 'Maybe' type to account for the fact that the empty
   list ('[]') has no head or tail. -}

getHead :: [a] -> Maybe a
getHead []     = Nothing
getHead (x:xs) = Just x

getTail :: [a] -> Maybe [a]
getTail []     = Nothing
getTail (x:xs) = Just xs

{- These definitions illustrate a piece of Haskell "culture": it is
   common to use single letter variable names like 'x' for these kinds
   of generic functions where it really can stand for anything. Also,
   it is common to 'pluralise' the name used for the head of the list
   for the tail. So in these definitions, we have the head 'x' and the
   tail 'xs' (Eck-es).

   Since lists may be of arbitrary size, we need a way to write
   functions on them that can handle arbitrary size inputs. Just as we
   did this in the type definition by making 'List' refer to itself
   recursively, we do this in functions by making them refer to
   themselves recursively.

   Here is a function that computes the length of a list: -}

length :: [a] -> Integer
length []     = 0
length (x:xs) = 1 + length xs

{- The first case, 'length []' states that the length of the empty list
   is '0'.

   The second case states that the length of a list with a head and a
   tail is '1' plus the length of the tail.

   We can see how this works by writing out the steps involved in computing the length of a short list:

     length [1,2,3]
   = length (1:2:3:[])
   = 1 + length (2:3:[])
   = 1 + 1 + length (3:[])
   = 1 + 1 + 1 + length []
   = 1 + 1 + 1 + 0
   = 3

   In this first line, I translated from the syntatic sugar for lists
   to the "real" representation using ':' and '[]'. I then
   systematically applied the rules of the 'length' function to get to
   some arithmetic to be evaluated, which evaluates to the length of
   the list. -}

{- A similar function is one that computes the 'sum' of a list: -}

sumList :: [Integer] -> Integer
sumList []     = 0
sumList (x:xs) = x + sumList xs

{- Writing out the steps involved has a similar structure, but with
   different values:

        sumList [1,2,3]
      = sumList (1:2:3:[])
      = 1 + sumList (2:3:[])
      = 1 + 2 + sumList (3:[])
      = 1 + 2 + 3 + sumList []
      = 1 + 2 + 3 + 0
      = 6
-}


{-!      Part 1.5 : APPENDING AND REVERSING LISTS

  Two useful functions on lists are 'append' and 'reverse'. These will
  also give us an opporuntity to see how to use multiple functions to
  accomplish a task.

  'append' is intended to put two lists one after the other. So we
  should have:

    append [1,2] [3,4]

  evaluates to

    [1,2,3,4]

  To define this function, we think in terms of what we ought to do
  for all possible cases of input list.

  When the first list is empty, we have:

    append [] ys

  Appending the empty list with 'ys' ought to give us 'ys'.

  When the first list has a head and a tail, we have:

    append (x:xs) ys

  If we assume that we know how to append 'xs' and 'ys', then we are
  left with the problem of what to do with the 'x'. Since it was at
  the head of the first list, and the first list is going on the front
  of the second list, a reasonable thing to do is to put it on the
  head of the final list:

    x : append xs ys

  Putting this together, we get the following recursive definition: -}

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

{- We can check this does the right thing by stepping through the
   function for an example input:

        append [1,2] [3,4]
      = append (1:2:[]) [3,4]
      = 1 : append [2] [3,4]
      = 1 : 2 : append [] [3,4]
      = 1 : 2 : [3,4]
      = [1,2,3,4]

   Of course, we can also just run the function:

      *Week01> append [1,2] [3,4]
      [1,2,3,4]

-}

{- Another useful function on lists is 'reverse'. Reversing a list takes
   a list as input and produces a list, so the type is: -}

reverse :: [a] -> [a]

{- We look at the empty list case first. Reversing the empty list ought
   to just be the empty list: -}

reverse [] = []

{- Reversing a list with something in it is a bit more complex. Let's
   look at two examples:

      reverse [1,2,3]   = [3,2,1]
      reverse [4,5,6,7] = [7,6,5,4]

   Bearing in mind the reasoning we used in the append definition,
   let's look carefully at what happens to the value at the head of
   the input lists. In both cases, it moved to the end. Similarly, the
   next element is moved to the position just before the end, and so
   on. We can decompose both examples into the following structure,
   using a very informal (non Haskell) notation:

      reverse (x : xs) = "reversed xs" + x at the end

   Translating this definition into Haskell poses a problem: how do we
   put a value at the end of a list? The answer is to use the 'append'
   function to append it on the end: -}

reverse (x:xs) = append (reverse xs) [x]

{- We can check our working by stepping through the definition of
   reverse on a small input list:

        reverse [1,2,3]
      = reverse (1:2:3:[])
      = append (reverse (2:3:[]) [1]
      = append (append (reverse (3:[]) [2]) [1]
      = append (append (append (reverse []) [3]) [2]) [1]
      = append (append (append [] [3]) [2]) [1]
      = [3,2,1]

   And again, we can test it by running it:

      *Week01> reverse [1,2,3]
      [3,2,1]
-}

{- Looking at the trace of 'reverse', we can see that it has a flaw. It
   builds up a tower of 'append's. Evaluating an 'append' takes time
   proportional to the length of the first list. So if we do 'append's
   for every element in the input list we take time:

     1 + 2 + 3 + 4 + ... + n = n*(n-1)/2

   So the time taken is proportional to the square of the length of
   the input list. But we should be able to reverse a list in time
   proportional to just the length of the list! How can we do this?

   The answer is to rewrite 'reverse' by using an accumulator
   parameter that stores the reversed list are building as we
   go. Let's see how this works.

   We first define a function called 'fastReverseHelper'. This takes
   two arguments: the list to reverse, and an accumulator that
   represents the list that has already been reversed: -}

fastReverseHelper :: [a] -> [a] -> [a]
fastReverseHelper []     accum = accum
fastReverseHelper (x:xs) accum = fastReverseHelper xs (x:accum)

{- This definition may be a little obscure at first, but it helps to
   look at the a trace of how it works on a short list. We start with
   the list [1,2] to reverse, and the empty 'accum'ulator parameter:

        fastReverseHelper [1,2] []
      = fastReverseHelper [2] (1:[])
      = fastReverseHelper [] (2:1:[])
      = [2,1]

   To reverse a list, we write 'fastReverse' that calls
   'fastReverseHelper' an empty initial accumulator: -}

fastReverse :: [a] -> [a]
fastReverse xs = fastReverseHelper xs []

{- And we can test it:

      *Week01> fastReverse [1,2,3]
      [3,2,1]
      *Week01> fastReverse [5,4,3,2,1]
      [1,2,3,4,5]
-}


{-!      Part 1.6 : TAKE, DROP AND CHUNK


  Finally for this week, we'll look at two more useful list functions,
  and how to put them together. This will also naturally lead to the
  idea of nested lists of lists.

  The first function is 'take', which takes a number of elements from
  the front of a list. It is defined by pattern matching on the number
  of elements remaining to be taken: -}

take :: Integer -> [a] -> [a]
take 0 xs     = []
take n []     = []
take n (x:xs) = x : take (n-1) xs

{- So, when we 'take 0' we always return the empty list. If we 'take n',
   where 'n' is not 0, we look at the list. If the list is empty, then
   we just return the empty list. If the list is not empty, we return
   a list with 'x' at the head, and the result of 'take (n-1)' for the
   tail.

   A similar function is 'drop', which drops 'n' elements from the
   front of a list: -}

drop :: Integer -> [a] -> [a]
drop 0 xs     = xs
drop n []     = []
drop n (x:xs) = drop (n-1) xs

{- We can put these together to write a function that turns a list into
   a list of "chunks". We will write a function with the following
   type: -}

chunk :: Integer -> [a] -> [[a]]

{- The function 'chunk' takes an 'Integer', which is the chunk size, and
   a list of 'a's. It returns a list of lists of 'a's (note the *two*
   pairs of square brackets, meaning a list of lists).

   The idea is that 'chunk n xs' will split 'xs' into chunks of size
   'n'. For example:

      chunk 2 [1,2,3,4,5]

   should return

      [[1,2],[3,4],[5]]

   (notice how there wasn't enough to fill the last chunk, so we
   return a partial chunk.)

   We now think about how we will write this function. First, we think
   about the empty list case. Turning the empty list into a list of
   chunks yields ... the empty list of chunks: -}

chunk n [] = []

{- If the list isn't empty, then we create a new list element (using
   ':'). In the head position, we place the new chunk made by 'take n'
   to get the first 'n' elements of the list. The tail of the output
   list is constructed by calling chunk recursively on the result of
   dropping the first 'n' elements: -}

chunk n xs = take n xs : chunk n (drop n xs)

{- To understand this function, try writing out what it does step by
   step on some small examples. I'll start you off by showing an
   example where it doesn't work so well. If the chunk size is '0',
   then it generates the empty chunk over and over again, forever:

        chunk 0 [1,2,3,4,5]
      = take 0 [1,2,3,4,5] : chunk 0 (drop 0 [1,2,3,4,5])
      = [] : chunk 0 [1,2,3,4,5]
      = [] : [] : [] : [] : ...
-}


{----------------------------------------------------------------------}
{- Tutorial Questions                                                 -}
{----------------------------------------------------------------------}

{- In the questions below, replace 'undefined' with your answers. Use
   GHCi to test them.-}

{- 1. Write a function: -}

isHorizontal :: Direction -> Bool
isHorizontal = undefined

{- that returns 'True' if the direction is 'Left' or 'Right', and
   'False' otherwise. -}


{- 2. Write a function: -}

flipHorizontally :: Direction -> Direction
flipHorizontally = undefined

{- that flips horizontally (Left <-> Right, Up and Down stay the same). -}


{- 3. Rewrite 'equalDirections' to take a 'Pair Direction Direction' as
      input: -}

pairOfEqualDirections :: Pair Direction Direction -> Bool
pairOfEqualDirections = undefined


{- 4. Define a datatype 'Triple a b c' for values that have three
      components. Write functions 'get1of3 :: Triple a b c -> a',
      'get2of3' and 'get3of3' that return the first, second and third
      components. You will have to come up with the type signatures
      for the second and third one. -}


{- 5. Pattern matching on specific characters is done by writing the
      character to match. For example: -}

isA :: Char -> Bool
isA 'A' = True
isA _   = False

{-    Write a function 'dropSpaces' :: [Char] -> [Char]' that drops
      spaces from the start of a list of characters. For example, we
      should have:

         *Week01> dropSpaces "   hello"
         "hello"

      (Strings in Haskell are really lists of 'Char's) -}

dropSpaces :: [Char] -> [Char]
dropSpaces = undefined

{- 6. Using 'reverse' and 'dropSpaces', write a function that removes
      spaces at the *end* of a list of characters. For example:

         *Week10> dropTrailingSpaces "hello    "
         "hello"
-}

dropTrailingSpaces :: [Char] -> [Char]
dropTrailingSpaces = undefined
