{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week03Solutions where

import Data.Char
import Prelude hiding ((.), map, filter, id, flip)

{-    WEEK 3 : HIGHER ORDER FUNCTIONS

   This week we will look at the concept of "functions as values".

   Treating functions as values is useful for turning programs that
   solve one specific problem into more general programs that solve
   whole classes of problems.

   Haskell programs can pass values like integers, strings, lists and
   trees to and from functions, and store them in structures like
   lists and trees. Haskell treats functions no differently from any
   other kind of data: functions can be returned as the result of
   functions, passed into functions, and stored in data structures. -}


{-    Part 3.1 : FUNCTIONS THAT RETURN FUNCTIONS

   First, we will look at how functions can return functions as
   results.

   We've already seen many functions that take several arguments. An
   example is 'add', which adds two 'Int's and returns an 'Int': -}

add :: Int -> Int -> Int
add x y = x + y

{- We write the type of a function that takes two arguments like so:

        t1 -> t2 -> t3

   What we've not mentioned so far is that this is really shorthand
   notation for the following type with parentheses inserted:

        t1 -> (t2 -> t3)

   Remembering that 'a -> b' is the type of functions that take 'a's
   and return 'b's, we can read this type as the type of "functions
   that take 't1's and return functions that take 't2's and return
   't3's.

   Therefore, the add function "takes an 'Int' and returns a function
   that takes an(other) 'Int' and returns an 'Int'.

   Once we see that 'add' is really a function that returns a
   function, we can see that we needn't always give it two
   arguments. We can define the 'addTen' functions by only giving
   'add' one of its arguments: -}

addTen :: Int -> Int
addTen = add 10

{- 'addTen' has type 'Int -> Int', even though we didn't write an
   argument name on the left side of the '='s, because 'add' has type
   'Int -> (Int -> Int)' and we've given an 'Int', leaving 'Int ->
   Int'. We could also write 'addTen' giving an explicit name for the
   argument, which we pass on to 'add 10'. This gives 'addTen2', which
   is equivalent to 'addTen': -}

addTen2 :: Int -> Int
addTen2 x = add 10 x

{- We can see even more clearly that multi-argument functions in Haskell
   work by taking one argument and returning a function by writing out
   a definition of 'add' using the '\x -> E' notation for
   functions.

   (The backslash '\' is meant to be an ASCII representation of a
   Greek lambda, because 'lambda' is a commonly used notation for
   writing anonymous functions.)

   An expression of the form '\x -> E' stands for "a function that
   takes an argument, which we call 'x', and returns 'E'". We write
   out 'add' in this form like so: -}

add2 :: Int -> (Int -> Int)
add2 = \x -> (\y -> x + y)

{- (Look at the way the bracketing in the program matches the bracketing
   in the type!)

   As a shorthand, we can avoid writing things like "\x -> (\y -> (\z ->
   ..." and instead write all the argument names together before the
   "->": -}

add3 :: Int -> (Int -> Int)
add3 = \x y -> x + y

{- The `\`/lambda notation also accepts patterns as well as argument
   names, as long as there is only one pattern. For example, pattern
   matching against pairs: -}

fst2 :: (a,b) -> a
fst2 = \(a,b) -> a

{- (Look at the coincidence between the type and the program!)

   The '\' or 'lambda' notation for functions may seem a bit pointless
   so far. Everything we've written using this notation could have
   been written more simply by placing the argument names to the left
   of the '='s. The advantage of the '\' lambda notation is that it
   allows us to write functions without needing to give them
   names. We'll see why this is important after we look at functions
   that take other functions as input. -}

{- One useful consequence of the way that Haskell represents
   multi-argument functions as functions that return other functions
   is that we can use functions that are applied to fewer arguments
   than they are expecting to reuse a function to create other
   functions.

   For example, if we make a function that takes two strings and
   appends them with a colon between them, then we can use this to
   prefix messages with what kind of message they are: -}

prefixWith :: String -> String -> String
prefixWith prefix str = prefix ++ ": " ++ str

{- For example:

      *Week03> prefixWith "ERROR" "Building on fire"
      "ERROR: Building on fire"
      *Week03> prefixWith "LOG" "Building burnt down"
      "LOG: Building burnt down"

   To avoid having to repeatedly say "ERROR" and "LOG", with the
   possibilty that we might mistype it occasionally, we can use
   partially applied functions to create specialised versions of
   'prefixWith' for error and log messages: -}

errorMessage :: String -> String
errorMessage = prefixWith "ERROR"

logMessage :: String -> String
logMessage = prefixWith "LOG"

{- And use them like this to get more readable code:

      *Week03> errorMessage "Building on fire"
      "ERROR: Building on fire"
      *Week03> logMessage "Building burnt down"
      "LOG: Building burnt down"
-}


{-    Part 3.2 : FUNCTIONS THAT TAKE FUNCTIONS AS INPUTS

   As I said in the introduction, Haskell treats functions as it does
   any other kind of value. The can be returned by functions, as we
   saw in Part 1. We'll now look at how and why Haskell functions can
   take functions as arguments.

   Let's start by looking at a simple definition. Here is a definition
   of the number 'ten' by adding '5' to itself: -}

ten :: Int
ten = add 5 5

{- We could think to ourselves "there's nothing special about the number
   '5' here, we could be adding any number to itself". So we move from
   the specific '5' to a general 'x', which we make an argument of the
   function. We now have a function that takes an 'Int' and returns an
   'Int': -}

double :: Int -> Int
double x = add x x

{- Continuing this line of thought, we think to ourselves "there's
   nothing special about 'add'ing here, we could use any operation
   that takes two 'Int's and returns an 'Int'". So we move from the
   specific 'add' to a general 'f', which we make an argument of the
   function. We adjust the type again: 'add' has type 'Int -> Int ->
   Int', so our new function takes a value of this type and returns a
   function that takes 'Int's and returns 'Int's: -}

applyCopy :: (Int -> Int -> Int) -> Int -> Int
applyCopy f x = f x x

{- 'applyCopy' is now a generally applicable function that takes *any*
   two argument function on 'Int's, and *any* 'Int' and passes that
   'Int' twice to the given function.

   We call 'applyOrder' a *higher order* function because it takes a
   function as its argument. The order of a function refers to how
   'functiony' its arguments are: A value with no arguments is of
   order 0, a function with arguments that have no arguments is order
   1, a function with arguments that take arguments is order 2, and so
   on.

   Because we have constructed 'applyCopy' by repeated moves from the
   specific to the general, we can get back down to earth again by
   applying 'applyCopy' to the original specific 'add' and '5'. So we
   can recover the 'double' function by applying 'applyCopy' to 'add': -}

double2 :: Int -> Int
double2 = applyCopy add

{- And we can recover 'ten' by applying 'double2' to '5': -}

ten2 :: Int
ten2 = double2 5

{- When we moved from 'ten' to 'applyCopy' above, we didn't change the
   types much: in the end, 'applyCopy' still worked on 'Int's. In the
   example, we will see how moving from specific functions to more
   general ones allows us to also make the types more general too.

   The 'quadruple' function applies 'double' twice to double a number: -}

quadruple :: Int -> Int
quadruple x = double (double x)

{- As above, there is nothing special about 'double' here. We move from
   the specific 'double' to a general 'f' to make the 'twice'
   function, which applies a function to an argument, and then applies
   it again. Only this time, we also make the type more general --
   t
   once without needing to think of a name. We can write 'double' as
   '\x -> x + x', which in some contexts may be clearer than the word
   'double'. -}

twice :: (a -> a) -> a -> a
twice f x = f (f x)

quadruple2 :: Int -> Int
quadruple2 = twice (applyCopy add)
      --     twice double
      --     twice (\x -> x + x)
      --     twice (\x -> 2 * x)

{- Because 'twice' is more general than 'quadruple', we can use it again
   for new purposes. For example, sexdecupling (ie, multiplying by 16) :-}

sexdecuple :: Int -> Int
sexdecuple = twice quadruple

{- 'twice' can be applied to any function whose return type is the same
   as its input type. For example, reversing twice is the same as
   doing nothing:

       *Week03> twice reverse [1,2,3,4]
       [1,2,3,4]

   But adding 10 twice adds 20:

       *Week03> twice (\x -> x + 10) 0
       20
-}


{-    Part 3.3 : MAP AND FILTER -}

{- One of the most useful places to use higher-order functions is to
   make general functions for processing containers full of
   data. Here, we will concentrate on lists. Let's see how to make
   some reusable functions on lists by following the same
   specific-to-general methodology that we did above.

   Here is a function that doubles every element of a list of
   integers: -}

doubleAll :: [Int] -> [Int]
doubleAll []     = []
doubleAll (x:xs) = double x : doubleAll xs

{- As above, we not that there is nothing special about use of the
   'double' function here. So we can move from the specific 'double'
   to the general 'f'. This gives us a general function that applies
   'f' to every element of a list, giving a new list of transformed
   elements. The name 'map' is the traditional name for this function: -}

map :: (a -> b) -> [a] -> [b]
map f [] = []
map f (x:xs) = f x : map f xs

{- We get back to 'doubleAll' by applying 'map' to 'double': -}

doubleAll2 :: [Int] -> [Int]
doubleAll2 = map double

{- 'map' allows us to "lift" any function acting on things to a function
   acting on lists of those things. For instance, taking the first
   value of a list of pairs by mapping 'fst' across the list: -}

fsts :: [(a,b)] -> [a]
fsts = map fst

{- Or pairing strings with their lengths: -}

withLengths :: [String] -> [(String, Int)]
withLengths = map (\s -> (s, length s))

{- Another useful higher-order function on lists is 'filter'. This
   function filters the input list to only keep the elements that
   match some condition. The condition is provided as a function of
   type 'a -> Bool', where 'a' is the type of elements of the
   list. Instead of working from specific-to-general as we did above
   we give the function 'filter' directly: -}

filter :: (a -> Bool) -> [a] -> [a]
filter p [] = []
filter p (x:xs)
  | p x       = x : filter p xs
  | otherwise = filter p xs

{- Now we can use 'filter' to quickly write a function that only keeps
   the even numbers in a list. -}

onlyEvens :: [Int] -> [Int]
onlyEvens = filter (\x -> x `mod` 2 == 0)

{- The functions 'map' and 'filter' are a useful pair of tools for
   building functions that work on lists, without having to write
   similar looking code over and over again.

   For example, in the quickSort function from Week 02, we had:

     qsort :: Ord a => [a] -> [a]
     qsort []     = []
     qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
        where smaller = [ y | y <- xs, y < x ]
              larger  = [ y | y <- xs, y >= x ]

   we can rewrite the two list comprehensions as filters instead: -}

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = filter (\y -> y < x) xs
        larger  = filter (\y -> y >= x) xs

{- Which of these is clearer depends on the reader ;) -}


{-     Part 3.4 : COMPOSITION AND IDENTITY

   Let's start by looking at one of the questions from the Week 01
   tutorial. You were asked to write 'dropSpace' and then use it to
   write 'dropTrailingSpaces'. One way of writing them is like this: -}

dropSpaces :: String -> String
dropSpaces []       = []
dropSpaces (' ':xs) = dropSpaces xs
dropSpaces xs       = xs

dropTrailingSpaces :: String -> String
dropTrailingSpaces xs = reverse (dropSpaces (reverse xs))

{- 'dropTrailingSpaces' is an example of function composition: making a
   function by plugging functions together so that output of one goes
   into the input of another. Here we have three functions composed
   together: 'reverse', then 'dropSpaces', and 'reverse' (reading
   right to left).

   This is such a common pattern, that it is worth writing another
   function that takes functions as input to capture it: -}

compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

{- 'compose' takes a function 'f :: b -> c', a function 'g :: a -> b',
    and a value 'x :: a' then feeds 'x' to 'g' to get a 'b' value, and
    then feeds that value to 'f' to finally get a 'c' value.

   We can rewrite 'dropTrailingSpaces' in terms of compose like so: -}

dropTrailingSpaces2 :: String -> String
dropTrailingSpaces2 = compose reverse (compose dropSpaces reverse)

{- which makes clear that it is the composition of three functions.

   Writing it like this isn't much clear as it is, but 'compose' is so
   useful that the Haskell standard library calls it '.', and it is
   written infix (in between its arguments). The '.'  is meant to
   mimic in ASCII the mathematical circle notation for function
   composition.

   Here is a definition of '.', written using the '\'/lambda notation: -}

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

{- With this, we get the following clearer definition of
   'dropTrailingSpaces': -}

dropTrailingSpaces3 :: String -> String
dropTrailingSpaces3 = reverse . dropSpaces . reverse

{- Function composition is especially useful for creating 'pipelines'
   that plug together several basic functions for processing lists
   into a larger list processing function. The concept is similar to
   the idea of Unix pipelines that plug together small programs that
   do one thing into larger units.

   An example Unix pipeline is the following. The 'grep' ("global
   regular expression") program searches for lines that match some
   pattern (here "CS316"), and the 'cut' program extracts certain
   fields from each line (here "-f1" indicates that we want the first
   field).

      grep CS316 | cut -f1 < registered-students.txt

    In Haskell, we replace 'grep' with 'filter', and 'cut' with 'map
    fst' to get the following, where we've used function composition
    '(.)' to plug together the basic functions. Note that Haskell
    pipelines go right to left, unlike Unix pipelines, which go left
    to right. -}

pipeline :: [(String,Int)] -> [String]
pipeline = map fst . filter (\(s,i) -> s == "CS316")

{- Another example uses 'wc -l' to count the number of lines in the
   output of 'grep':

      grep CS316 registered-students.txt | wc -l

   We can mimic this by using 'length': -}

pipeline2 :: [(String,Int)] -> Int
pipeline2 = length . filter (\(s,i) -> s == "CS316")


{- Function composition is a form of 'multiplication' for functions: we
   can 'multiply' two functions by composing them. (If you know about
   Linear Algebra and Matrices, then think about how multiplying two
   matrices has the same effect as composing the linear functions that
   they represent.)

   So what is the equivalent of '1'?

   It is identity function, which "does nothing" by returning its
   argument unaffected: -}

id :: a -> a
id x = x


{-     Part 3.5 : MAP FOR OTHER DATATYPES

   We can think of 'map' for lists as taking an input list full of
   values, applying some function to every value, and the returning a
   list *of the same shape* with the transformed values
   in. Graphically, we have:

          [ a1, a2, ..., an ]
            |   |        |
            v   v        v
          [ b1, b2, ..., bn ]

   where b1 = f a1, b2 = f a2, ..., bn = f an.

   We can apply the same idea to other kinds of data types that act
   like containers. For instance, if we define 'Tree's like so: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- Then we can draw the same idea of transforming values and maintaining
   shape:

      Node (Node Leaf a1 Leaf) a2 (Node Leaf a3 Leaf)
                      |        |             |
                      v        v             v
      Node (Node Leaf b1 Leaf) b2 (Node Leaf b3 Leaf)

   where, again, b1 == f a1, and so on.

   So, 'map' for 'Tree's seems to make sense. Let's try to write it as
   a function: -}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f Leaf         = Leaf
mapTree f (Node l x r) = Node (mapTree f l) (f x) (mapTree f r)

{- This is structurally similar to 'map' for lists:

    1. The empty tree ('Leaf') gets sent to the empty tree

    2. 'Node's are translated to 'Node's, with their contained values
       transformed.

  Giving it a go, we can see that it takes trees to trees,
  transforming all the contained values:

     *Week03> mapTree (\x -> x + 1) (Node (Node Leaf 3 Leaf) 0 (Node Leaf 7 Leaf))
     Node (Node Leaf 4 Leaf) 1 (Node Leaf 8 Leaf)
-}

{- We can also apply the same idea to 'Maybe's. We can think of 'Maybe'
   as a container that contains either zero or one element. So
   'mapMaybe' should map 'Nothing' to 'Nothing' (the empty container),
   and 'Just x' to 'Just (f x)': -}

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe f Nothing  = Nothing
mapMaybe f (Just a) = Just (f a)

{- The containers we've seen so far have been made from constructors. We
   can also use first class functions to make containers that contain
   infinitely many things. The following definition makes a datatype
   of values that contain an 'a' for every possible 'String': -}

data WithString a
  = MkWithString (String -> a)

{- For instance, we can make a 'WithString Int' that, for every string,
   contains its length: -}

lengths :: WithString Int
lengths = MkWithString (\s -> length s)

{- And we can look up the value associated with a certain string by
   using the following function: -}

valueOf :: String -> WithString a -> a
valueOf s (MkWithString f) = f s

{- So:

     *Week03> valueOf "hello" lengths
     5
     *Week03> valueOf "hi" lengths
     2

   Because it is a container, 'WithString' also has a 'map'
   function. If we have a container of 'a's for every 'String', and a
   function 'f :: a -> b', then we can get a container of 'b's for
   every 'String', by using 'f' to transform the values returned by
   the first container: -}

mapWithString :: (a -> b) -> WithString a -> WithString b
mapWithString f (MkWithString h) = MkWithString (\s -> f (h s))

{- Now we can transform the 'lengths' container to one that contains
   'True' for all strings of length greater than 4 and 'False' for
   those where it is less than 4:

      *Week03> valueOf "hello" (mapWithString (\x -> x > 4) lengths)
      True
      *Week03> valueOf "hi" (mapWithString (\x -> x > 4) lengths)
      False
-}

{- It may seem from the examples above that any parameterised datatype
   acts like a container, and has a 'map'-like function. Here is an
   example where this is not the case.

   If we define a datatype of functions from a type to itself, like
   so: -}

data Fun a = MkFun (a -> a)

{- This type is useful as a way of capturing the idea of transformers
   that return values of the same type as they accept. We will see it
   return when we talk about Monoids in Week 05.

   But can we write a 'map' for this type?

   If we start with:

     mapFun :: (a -> b) -> Fun a -> Fun b
     mapFun f (MkFun h) = MkFun ????

   then the '???' requires us to write something of type 'b ->
   b'. However, what we have are:

     - f :: a -> b
     - h :: a -> a

   Neither of these takes a 'b' as input, so there is no way to use
   them to write a function of type 'b -> b'. In fact, pretty much the
   only function of type 'b -> b' we can use is the identity function
   (ignoring the function that never terminates): -}

mapFun :: (a -> b) -> Fun a -> Fun b
mapFun f (MkFun h) = MkFun id

{- This seems to be a valid definition of a 'map' for the type
   constructor 'Fun'. But somehow it doesn't seem right: if we
   intuitively think of fmap as altering all of the values stored in a
   container whilst maintaining the structure, then it seems odd to
   always return the same answer -- the 'id' function in this case.

   To exclude this kind of dodgy definition, we require that 'map'
   functions should always obey two equational laws that intuitively
   state that 'map' does do modification of values and not the shapes
   of structures.

   The laws are:

      1. map id c == c

         Mapping the identity function over a container should not
         affect the container or its values at all. This is reasonable
         -- if we do nothing to the values stored in the container,
         then the whole thing should be unaffected.

      2. map f (map g c) == map (f . g) c

         If we map a function 'g' over a container, and then map a
         function 'f' over the result, then that ought to be the same
         as just mapping their composition. Again, this is reasonable:
         if we are leaving the structure of a container untouched,
         then it shouldn't matter how many times we traverse over it
         to alter the values stored in it.

   We can now see that 'mapFun' for 'Fun' defined above fails the
   first law. We have:

      fmap id (MkFun g) = MkFun id

   but, to satisfy the first law, the result ought to be 'MkFun g'.

   Going back, it is possible to check that the other 'map' functions
   we wrote above all satisfy the two laws above, so they are "proper"
   'map' functions. -}


{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- 1. Lambda notation.

   Rewrite the following functions using the '\x -> e' notation (the
   "lambda" notation), so that they are written as 'double =
   <something>', and so on. -}

mulBy2 :: Int -> Int
mulBy2 = \x -> 2*x

mul :: Int -> Int -> Int
mul = \x y -> x * y

invert :: Bool -> Bool
invert = -- \x -> if x then False else True
         -- not
         \x -> case x of
                 True  -> False
                 False -> True
  {- HINT: use a 'case', or an 'if'. -}


{- 2. Partial Application

   The function 'mul' defined above has the type 'Int -> Int ->
   Int'. (a) What is the type of the Haskell expression:

       mul 10

   (b) what is 'mul 10'? How can you use it to multiply a number? -}


{- 3. Partial Application

   Write the 'mulBy2' function above using 'mul'. Can you make your
   function as short as possible? -}

double_v2 :: Int -> Int
double_v2 = mul 2

{- 4. Using 'map'.

   The function 'toUpper' takes a 'Char' and turns lower case
   characters into upper cases one. All other characters it returns
   unmodified. For example:

       > toUpper 'a'
       'A'
       > toUpper 'A'
       'A'

   Strings are lists of characters. 'map' is a function that applies a
   function to every character in a list and returns a new list.

   Write the function 'shout' that uppercases a string, so that:

      > shout "hello"
      "HELLO"
-}

shout :: String -> String    -- remember that String = [Char]
shout = map toUpper


{- 5. Using 'map' with another function.

   The function 'concat' does what the function 'concatLists' from
   Exercise 1 does, but is built in to the library:

      > concat [[1,2],[3,4],[5,6]]
      [1,2,3,4,5,6]

   Using 'map', 'concat', and either a helper function or a function
   written using '\', write a function 'dupAll' that duplicates every
   element in a list. For example:

      > dupAll [1,2,3]
      [1,1,2,2,3,3]
      > dupAll "my precious"
      "mmyy  pprreecciioouuss"

   HINT: try writing a helper function that turns single data values
   into two element lists. -}

dupAll :: [a] -> [a]
-- dupAll = concat . map (\x -> [x,x])
dupAll xs = concat (map (\x -> [x,x]) xs)

-- [1,2,3]
-- [[1,1],[2,2],[3,3]]
-- [1,1,2,2,3,3]


{- 6. Using 'filter'

   (a) Use 'filter' to return a list consisting of only the 'E's in
       a 'String'.

   (b) Use 'onlyEs' and 'length' to count the number of 'E's in a string.

   (c) Write a single function that takes a character 'c' and a string
       's' and counts the number of 'c's in 's'. -}

onlyEs :: String -> String
onlyEs = filter (\x -> x == 'E')

numberOfEs :: String -> Int
numberOfEs xs = length (onlyEs xs)
           --  length . onlyEs

numberOf :: Char -> String -> Int
numberOf c = length . filter (\x -> x == c)


{- 7. Rewriting 'filter'

   (a) Write a function that does the same thing as filter, using
      'map' and 'concat'.

   (b) Write a function that does a 'map' and a 'filter' at the same
       time, again using 'map' and 'concat'.
-}

--   ['E',  'I','E',   'I','O']
--   [['E'],[], ['E'], [], [] ]
--   ['E',      'E'           ]

filter_v2 :: (a -> Bool) -> [a] -> [a]
filter_v2 p = concat . map (\x -> if p x then [x] else [])

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap p = concat . map (\x -> case p x of
                                    Nothing -> []
                                    Just y  -> [y])


{- 8. Evaluating Formulas

   Here is a datatype describing formulas in propositional logic, as
   in CS208 last year. Atomic formulas are represented as 'String's. -}

data Formula
  = Atom String
  | And  Formula Formula
  | Or   Formula Formula
  | Not  Formula
  deriving Show

{- (a) Write a function that evaluates a 'Formula' to a 'Bool'ean value,
       assuming that all the atomic formulas are given the value
       'True'. Note that the following Haskell functions do the basic
       operations on 'Bool'eans:

           (&&) :: Bool -> Bool -> Bool    -- 'AND'
           (||) :: Bool -> Bool -> Bool    -- 'OR'
           not  :: Bool -> Bool            -- 'NOT'
-}

eval_v1 :: Formula -> Bool
eval_v1 (Atom a)  = True
eval_v1 (And p q) = eval_v1 p && eval_v1 q
eval_v1 (Or p q)  = eval_v1 p || eval_v1 q
eval_v1 (Not p)   = not (eval_v1 p)




{- (b) Now write a new version of 'eval_v1' that, instead of evaluating
       every 'Atom a' to 'True', takes a function that gives a 'Bool'
       for each atomic proposition: -}

eval :: (String -> Bool) -> Formula -> Bool
eval v (Atom a)  = v a
eval v (And p q) = eval v p && eval v q
eval v (Or p q)  = eval v p || eval v q
eval v (Not p)   = not (eval v p)

{- For example:

     eval (\s -> s == "A") (Or (Atom "A") (Atom "B"))  == True
     eval (\s -> s == "A") (And (Atom "A") (Atom "B")) == False
-}


{- 9. Substituting Formulas

   Write a function that, given a function 's' that turns 'String's
   into 'Formula's (a "substitution"), replaces all the atomic
   formulas in a Formula with whatever 'f' tells it to: -}

subst :: (String -> Formula) -> Formula -> Formula
subst v (Atom a)  = v a
subst v (And p q) = subst v p `And` subst v q
subst v (Or p q)  = subst v p `Or` subst v q
subst v (Not p)   = Not (subst v p)

{- For example:

     subst (\s -> if s == "A" then Not (Atom "A") else Atom s) (And (Atom "A") (Atom "B")) == And (Not (Atom "A")) (Atom "B")
-}

{- 10. Composition

   Write a function '>>>' that composes two functions: takes two
   functions 'f' and 'g', and returns a function that first runs 'f'
   on its argument, and then runs 'g' on the result.

   HINT: this is similar to the function 'compose' above. -}

(>>>) :: (a -> b) -> (b -> c) -> a -> c
(>>>) f g x = g (f x)

{- Try rewriting the 'numberOfEs' function from above using this one. -}

{- 11. Backwards application

   Write a function of the following type that takes a value 'x' and a
   function 'f' and applies 'f' to 'x'. Note that this functions takes
   its arguments in reverse order to normal function application! -}

(|>) :: a -> (a -> b) -> b
(|>) x f = f x


{- This function can be used between its arguments like so:

       "HELLO" |> map toLower

   and it is useful for chaining calls left-to-right instead of
   right-to-left as is usual in Haskell:

       "EIEIO" |> onlyEs |> length
-}

{- 12. Flipping

   Write a function that takes a two argument function as an input,
   and returns a function that does the same thing, but takes its
   arguments in reverse order: -}

flip :: (a -> b -> c) -> b -> a -> c
flip f a b = f b a
