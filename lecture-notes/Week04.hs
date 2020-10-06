{-# LANGUAGE ParallelListComp #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week04 where

import Prelude hiding (foldr, foldl, Maybe (..), Left, Right, filter, zip)

{-    WEEK 04 : PATTERNS OF RECURSION

   In Week 03 we looked at some ways that higher order functions can
   be used to combine common patterns of behaviour into single
   functions that are specialised by supplying the right functions to
   them. Our main examples were abstracting the idea of applying a
   function to every element of a list to get a new list ('map') and
   filtering a list based on some condition ('filter').

   We will now look at some more examples of higher order functions
   that capture common patterns. The key common factor is patterns of
   recursion that follow the structure of a datatype. This captures
   the pattern of "doing something" to every node in a datastructure,
   similar to the idea of the Visitor pattern in OO languages. -}


{-    Part 4.1 : FOLDING RIGHT

   Most of the functions we have written so far in this course have
   been recursive, because this is the way that Haskell deals with
   data of arbitrary size. In Week 01, we saw the 'total' function,
   which sums up the integers in a list of integers: -}

total :: [Int] -> Int
total []     = 0
total (x:xs) = x + total xs

{- Another function we've seen, or used, a few times is 'len', which
   computes the length of a list, again using recursion: -}

len :: [a] -> Int
len []     = 0
len (x:xs) = 1 + len xs

{- Looking at these two definitions, we can see that they are quite
   similar. They both do something with the empty list (they both
   return '0'), and they compute their value for a list with head 'x'
   and tail 'xs' by computing a value from the tail and then doing
   something with that value. For 'total' it is added to 'x'; for
   'len', '1' is added to it.

   Another example of a similar function, albeit in a slightly
   obfuscated form, is 'append': -}

append :: [a] -> [a] -> [a]
append []     ys = ys
append (x:xs) ys = x : append xs ys

{- The value 'ys' doesn't change throughout the execution of this
   function, so we can rewrite it using a 'where' clause, like so to
   separate out arguments that are changed between calls to 'append'
   and those that are fixed: -}

append' :: [a] -> [a] -> [a]
append' xs ys = appendHelper xs
  where appendHelper []     = ys
        appendHelper (x:xs) = x : appendHelper xs

{- Now we can see that 'appendHelper' looks very much like the 'total'
   and 'len' functions above: there is a thing to do in the '[]' case
   (return 'ys'), and there is a thing to do in the 'x:xs' case
   that is defined in terms of the result of processing 'xs'.

   Now we take the idea from Lecture 05 replacing the specific parts
   of these functions with parameters, to make a generic version of
   all these functions that can be specialised to each one of them.

   Let's start with 'total', as it is perhaps the simplest one. If we
   first make a generic version by turning the specific '0' into an
   argument, then we get a new function that effectively computes the
   sum of a list, plus some initial value: -}

totalPlus :: Int -> [Int] -> Int
totalPlus a []     = a
totalPlus a (x:xs) = x + totalPlus a xs

{- Now we go another step, and generalise from _adding_ the head on to
   the result of processing the rest of the list to performing some
   arbitrary operation that we take as a parameter 'f'. The resulting
   function is called 'foldr' in the Haskell library (short for "fold
   right"): -}

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f a []     = a
foldr f a (x:xs) = f x (foldr f a xs)

{- We can now recover 'total' by specialising with the operation being
   '+' and the initial value being '0': -}

total' :: [Int] -> Int
total' xs = foldr (+) 0 xs

{- And we can recover 'len' by specialising with the operation being
   "add one" (ignoring the actual value of 'x') and the initial value
   being '0' again: -}

len' :: [a] -> Int
len' xs = foldr (\x l -> 1 + l) 0 xs

{- We can now quickly write new recursive functions on lists, simply by
   saying what the operation is, and what to do for the empty
   list. For example, computing the product of a list (multiplying all
   the elements) by saying that the operation is '*' and the initial
   value is '1'. -}

product' :: [Int] -> Int
product' xs = foldr (*) 1 xs

{- Similarly, we can rewrite 'append', where the operation we do at
   every stage is '(:)' ("cons"), and the initial value is the second
   list 'ys'. -}

append'' :: [a] -> [a] -> [a]
append'' xs ys = foldr (:) ys xs

{- 'foldr' is surprisingly powerful. In fact, it is possile to write all
   structurally recursive functions on lists (recursive functions that
   only make recursive calls on sublists of the input list) using
   foldr. We won't prove this here, but we can write some functions on
   lists we've seen already using 'foldr'.

   First up, let's rewrite the 'map' function from Lecture 05 using
   'foldr'. The original 'map' looks like: -}

mapO :: (a -> b) -> [a] -> [b]
mapO f []     = []
mapO f (x:xs) = f x : mapO f xs

{- Looking at this function, we can see that it has the same structure
   as 'sum' and 'len' above: a particular value is returned for the
   '[]' case, and the 'x:xs' case is handled by combining 'x' and the
   _result_ of processing 'xs'. So we can write 'map' as a 'foldr': -}

map :: (a -> b) -> [a] -> [b]
map f = foldr (\a bs -> f a : bs) []

{- Similarly, the original 'filter' is written as: -}

filterO :: (a -> Bool) -> [a] -> [a]
filterO p []     = []
filterO p (x:xs) = if p x then x : filter p xs else filter p xs

{- Which translates into a 'filter' as: -}

filter :: (a -> Bool) -> [a] -> [a]
filter p = foldr (\a filtered -> if p a then a : filtered else filtered) []

{- EXERCISE: the following recursive function returns the list it is
   given as input: -}

listIdentity :: [a] -> [a]
listIdentity []     = []
listIdentity (x:xs) = x : listIdentity xs

{- Write this function as a 'foldr': -}

listIdentity' :: [a] -> [a]
listIdentity' = foldr undefined undefined

{- EXERCISE: the following recursive function does a map and a filter at
   the same time. If the function argument sends an element to
   'Nothing' it is discarded, and if it sends it to 'Just b' then 'b'
   is placed in the output list. -}

mapFilter :: (a -> Maybe b) -> [a] -> [b]
mapFilter f [] = []
mapFilter f (x:xs) = case f x of
                       Nothing -> mapFilter f xs
                       Just b  -> b : mapFilter f xs

{- Write this function as a 'foldr': -}

mapFilter' :: (a -> Maybe b) -> [a] -> [b]
mapFilter' f xs = foldr undefined undefined xs


{-    Part 4.2 : FOLDING LEFT

   Above, I said that the name 'foldr' is short for "fold right". What
   is "fold"ing? and why "right"?

   The answer lies in writing out what 'foldr' does on a general
   list. A call to 'foldr f a [x1,x2,x3]' expands like so:

          foldr f a [x1,x2,x3]
      ==  f x1 (foldr f a [x2,x3])
      ==  f x1 (f x2 (foldr f a [x3]))
      ==  f x1 (f x2 (f x3 (foldr f a [])))
      ==  f x1 (f x2 (f x3 a))

   We say that this is a fold of the list [x1,x2,x3] in the sense that
   it "folds" up the elements of the list using 'f'. The "right" comes
   from the fact that the bracketing is "to the right". This is
   perhaps clearer if we write the 'f's infix:

      ==  x1 `f` (x2 `f` (x3 `f` a))

   This inspires the question "what about bracketing this other way"?
   Is there a function that produces the bracketing:

          ((a `f` x1) `f` x2) `f` x3

   (Note that we've had to put the initial 'a' at the start now,
   instead of at the end.)

   There is! And it is called 'foldl' for "fold left". Let's look at
   the definition: -}

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f a []     = a
foldl f a (x:xs) = foldl f (f a x) xs

{- Just as for 'foldr', we take an initial value and an operation. The
   difference now is that instead of doing the operation on the
   element 'x' and the result of processing 'xs', we do the operation
   on the 'a' and 'x', passing the updated copy to the next step. So
   the second argument acts as an accumulator, building up a value,
   starting with 'a' and adding 'x' on to it with each step.

   Writing out 'foldl' on an example list gives us:

         foldl f a [x1,x2,x3]
     ==  foldl f (f a x1) [x2,x3]
     ==  foldl f (f (f a x1) x2) [x3]
     ==  foldl f (f (f (f a x1) x2) x3) []
     ==  (f (f (f a x1) x2) x3)

   and writing the last line out with 'f' infix gives:

     ==  ((a `f` x1) `f` x2) `f` x3

   as expected.

   'foldl' captures the pattern of iterating through a list, updating
   some piece of state as we go. As an illustrative example, let's
   take the 'Direction' data type from Week 01, and a Position type
   that represents positions as a pair of 'X' and 'Y' coordinates: -}

data Direction = Up | Down | Left | Right
  deriving Show

type Position = (Int, Int)

{- And define a 'move' function that takes a 'Position' and a
   'Direction' and moves the 'Position' by the 'Direction': -}

move :: Position -> Direction -> Position
move (x,y) Up    = (x,y+1)
move (x,y) Down  = (x,y-1)
move (x,y) Left  = (x-1,y)
move (x,y) Right = (x+1,y)

{- 'foldl' then "lifts" the single step 'move' function to working on
   lists of 'Direction's: -}

moves :: Position -> [Direction] -> Position
moves = foldl move

{- Here's an example: -}

steps :: [Direction]
steps = [Up, Left, Down]

{- Running 'moves (0,0) steps' will yield (-1,0) since we went:

      (0,0) --Up--> (0,1) --Left--> (-1,1) --Down--> (-1,0)
-}

{-    Part 4.3 : FOLDING OTHER DATA TYPES

   In Parts I and II, we've looked at folding over lists, from the
   right ('foldr') and from the left ('foldl'). The basic idea we
   started with when creating 'foldr' was to observe that recursive
   functions on lists often fit into the following pattern: they have
   a data value for the empty list, and an operation that combines the
   head of a list with the result of processing the tail. The two bits
   correspond to the two constructors for lists: '[]' and '(:)'
   ("cons").

   This correspondence between the element and the operation and the
   constructors of lists becomes apparent if we remember what 'foldr'
   did on the list '[x1,x2,x3]' we wrote out above, we get:

      x1 `f` (x2 `f` (x3 `f` a))

   compare this to how the list '[x1,x2,x3]' is constructed in terms of
   '[]' and '(:)':

      x1  :  (x2  :  (x3  :  []))

   Running 'foldr f a' on the list '[x1,x2,x3]' has had the effect of
   replacing all the '(:)'s in the list with 'f's, and the final '[]'
   with 'a'.

   Once we have this observation, we can apply it to any datatype
   definition in Haskell, to get a 'fold' operation. The 'fold' for a
   datatype 'X' will have an argument for each of 'X's constructors,
   telling the 'fold' what to do with that constructor. For example,
   here is the 'Tree' datatype again: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- 'Tree a' has two constructors, like lists, but this time the second
   constructor takes three arguments: the left subtree, the data, and
   the right subtree.

   Just as 'foldr' took as arguments what to do for '[]' and what to
   do for '(:)', 'foldTree' takes as arguments what to do for 'Leaf'
   and what to do for 'Node'. The major difference is that the
   operation for 'Node' now takes three arguments (because 'Node'
   takes tree arguments):

      1. The _result_ of processing the left subtree
      2. The data value stored at this node
      3. The _result_ of processing the right subtree

   and it returns the result of processing the current node.

   The definition of 'foldTree' is very similar to 'foldr': -}

foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
foldTree l n Leaf                = l
foldTree l n (Node left x right) = n (foldTree l n left) x (foldTree l n right)

{- As an example, just as we used 'foldr' to write 'total' for lists, we
   can use 'foldTree' to sum up the values stored in a 'Tree Int', by
   using '0' for the leaves, and adding up the three values for the
   nodes: -}

totalTree :: Tree Int -> Int
totalTree = foldTree 0 (\l x r -> l + x + r)

{- As another example, here is the 'Maybe' type we've seen several
   times: -}

data Maybe a
  = Nothing
  | Just a
  deriving (Eq, Show)

{- 'Maybe' isn't recursive, as lists and trees are, but we can still do
   the same constructor driven approach to making a general "recursion
   scheme" for it.

   From the constructors, 'foldMaybe' takes a value to use when the
   input is 'Nothing' and a function to apply to the 'x' in 'Just x': -}

foldMaybe :: b -> (a -> b) -> Maybe a -> b
foldMaybe n j Nothing  = n
foldMaybe n j (Just x) = j x

{- (This function is in the standard library under the name 'maybe')

   As an example of using this, here is the 'fromMaybe' function from
   the standard library module 'Data.Maybe', which takes a "default
   value" and a Maybe value. The default value is used when the Maybe
   value is Nothing: -}

fromMaybe :: a -> Maybe a -> a
fromMaybe x = foldMaybe x (\x -> x)

{- EXERCISE: The following is a datatype of Natural Numbers (whole
   numbers greater than or equal to zero), represented in unary. A
   natural number 'n' is represented as 'n' applications of 'Succ' to
   'Zero'. So '2' is 'Succ (Succ Zero)'. Using the same recipe as
   above, work out the type and implementation of a 'fold' function
   for 'Nat's. -}

data Nat
  = Zero
  | Succ Nat
  deriving Show

{- HINT: think about proofs by induction. A proof by induction has a
   base case and a step case. -}


{-    Part 4.4 : FOLD LEFT AND MONOIDS -}

{- FIXME: take material from Lec09

   Maybe leave until next week?
-}


{-    Part 4.5 : LIST COMPREHENSIONS

   FIXME: tidy this up, get rid of infinite lists and zip

   We have already seen that we can construct finite lists simply by
   giving an exhaustive list of their elements: -}

ex1 :: [Int]
ex1 = [2,4,6,8]

{- This matches the mathematical notation for giving a finite set by
   listing all it's elements explicitly:

      {2,4,6,8}

   Note that the traditional notation for sets in mathematics is to
   use curly braces {}, whereas we use square brackets [] for lists in
   Haskell.

   Sometimes it would be inconvenient to list all elements of a set,
   and we may trust the reader to work out which set is meant from an
   easily inferable pattern. For instance, we may write

     {2,4,..,120}

   for the set containing every even number between 2 and 120. Also
   Haskell allows this syntax, in limited cases. If you try evaluating -}

ex2 :: [Int]
ex2 = [2,4..120]

{- in GHCi, you will find that we indeed have defined a list containing
   every even number between 2 and 120:

      GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
      Prelude> :load "Lec06.hs"
      [1 of 1] Compiling Lec06            ( Lec06.hs, interpreted )
      Ok, modules loaded: Lec06.
      *Lec06> ex2
      [2,4,6,8,10,12,14,16,18,20,22,24,26,28,30,32,34,36,38,40,42,44,46,48,50,52,54,56,58,60,62,64,66,68,70,72,74,76,78,80,82,84,86,88,90,92,94,96,98,100,102,104,106,108,110,112,114,116,118,120]


   Note that we have to use exactly two dots, and no commas before or
   after the dots. The cleverness of Haskell spotting the pattern is also
   limited: the difference between the second and first element of the
   list will be used as the step length, and new elements are generated
   until we go past the last element:

      *Lec06> [1,5..11]
      [1,5,9]

   We can also make infinite lists by leaving out the upper bound:

      *Lec06> [4..]
      [4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51,52,53,54,55,56,57,58,59,60,61,62,63,64,65,66,67,68,69,70,71,72,73,74,75,76,77,78,79,80,81,82,83,84,85,86,87,88,89,90,91,92,93,94,95,96,97,98,99,100,101,102,103,104,105,106,107,108,109,110,111,112,113,114,115,116,117,118,119,120,121,122,123,124,125,126,127,128,129,130,131,132,133,134,135,136,137,138,139,140,141,142,143,144,145,146,147,148,149,150,151,152,153,154,155,156,157,158,159,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,192,193,194,195,196,197,198,199,200,201,202,203Interrupted

   Here we had to press Ctrl-C to stop printing out the list
   forever. Computing with such infinite data is possible because
   of /lazy evaluation/, which we will learn more about in Lecture
   17.

   Finally, we come to the comprehensions that the lecture title refers
   to. In mathematics, we can also form sets by describing the properties
   that the elements of the set should satisfy. For instance, we can form
   the set

     { x * 2 | x ∈ {0,1,2,3,4} }

   Forming such sets is called using /set comprehension/, because
   we can /comprehend/ the totality of the set just from the property
   it by definition satisfies. In Haskell, we have again similar
   notation for forming lists, which is called /list comprehension/: -}

ex3 :: [Int]
ex3 = [ x * 2 | x <- [0,1,2,3,4] ]

{- We see that ex3 evaluates to the list of even numbers

     [0,2,4,6,8]

   Here is another example of defining a list by comprehension:
-}

squares :: [Int]
squares = [ x ^ 2 | x <- [0..10] ]

{- Some terminology, and a pronunciation guide:

   * The symbol "|" is pronounced "such that". It separates the
     elements in the output list (on the left) from the properties
     (on the right) that they satisfy, and which define them.

   * The symbol "<-" is pronounced "comes from" or "drawn from". The
     phrase "x <- [0..10]" means that we let x range over the elements
     in the list [0..10]. This is the /generator/ of the comprehension. -}

{- There can be more than one generator, as the following example shows: -}

allpairs :: [(Int,Int)]
allpairs = [ (x, y) | x <- [0..5], y <- [4..6] ]

{- If we evaluate this, we see that we get all pairs (x, y), where x
   ranges from 0 to 5, and y ranges from 4 to 6:

      *Lec06> allpairs
      [(0,4),(0,5),(0,6),(1,4),(1,5),(1,6),(2,4),(2,5),(2,6),(3,4),(3,5),(3,6),(4,4),(4,5),(4,6),(5,4),(5,5),(5,6)]

   Note the order of the pairs: first we set x = 0, and let y range
   through all values between 4 and 6. Next we do x = 1, with y
   again ranging between 4 and 6, and so on. If we change the order
   of the generators -}

allpairsOtherorder :: [(Int,Int)]
allpairsOtherorder = [ (x, y) | y <- [4..6], x <- [0..5] ]

{- we see that we get the same elements as before, but in a different
   order:

      *Lec06> allpairsOtherorder
      [(0,4),(1,4),(2,4),(3,4),(4,4),(5,4),(0,5),(1,5),(2,5),(3,5),(4,5),(5,5),(0,6),(1,6),(2,6),(3,6),(4,6),(5,6)]

   This time we first let y range from 4 to 6 in the "outer loop", with
   x ranging from 0 to 5 for each fixed value of y in the "inner loop".
   If we were dealing with sets, we would consider these two expressions
   the same, but we are not; we are dealing with lists. Lists are equal
   if the have the same elements in the same order, and so, allpairs and
   allpairsOtherorder are /not/ the same. -}

{- Later generators can depend on the values of earlier ones: -}

ordpairs :: [(Int,Int)]
ordpairs = [ (x, y) | x <- [1..3], y <- [x..5] ]

{- Here the second component y will always be <= x, so this way
   we generate ordered pairs:

      *Lec06> ordpairs
      [(1,1),(1,2),(1,3),(1,4),(1,5),(2,2),(2,3),(2,4),(2,5),(3,3),(3,4),(3,5)]

   We can use this for a neat way to write the function which
   concatenates a list of lists: -}

concat :: [[a]] -> [a]
concat xss = [ x | xs <- xss, x <- xs]

{- See? For every list xs in the input list xss, and for every
   element x in the list xs, we put x in the output. -}

{- Just like on the left hand side of equations defining functions
   by pattern matching, if a a variable is not used, we do not need
   to name it, but can simply write an underscore _ to signal that we
   do not care about its value: -}

firsts :: [(a,b)] -> [a]
firsts ps = [ x | (x , _) <- ps ]

{- Alternatively, this can be written in the following way without
   using a pattern in the generator: -}

firsts' :: [(a,b)] -> [a]
firsts' ps = [ fst xy | xy <- ps ]

{- Here we first consider all pairs xy from the list ps, and apply
   the function fst : (a, b) -> a from the standard library to
   extract the first component. -}


{- Guards, guards! We can filter out elements we do not want in the
   output list by introducing a /guard/, i.e. a Boolean expression
   that has to evaluate to True for the element to be included. The
   following definition keeps only those integers x that divide n
   exactly -- in other words, the prime factors of n: -}

factors :: Int -> [Int]
factors n = [ x | x <- [1..n], n `mod` x == 0 ]

{- This makes it easy to write a primality test: by definition,
   an integer is prime if it is only divisible by 1 and itself: -}

prime :: Int -> Bool
prime 1 = False
prime n = factors n == [1,n]

{- (We have to treat 1 separately, because 1 should be considered
   a prime, but factors 1 == [1] /= [1,1]. Here one can see that
   one sometimes has to be careful when more or less thinking of
   lists as representations of sets -- because for sets, of course
   {1} == {1,1}.)

   It is now straightforward to use another guard to define an
   infinite list containing exactly all the prime numbers
   (computed in a not very efficient way): -}

primes :: [Int]
primes = [ x | x <- [1..], prime x ]

{- What if we also would like to keep track of the order of the
   prime? Naively, we could try something like this: -}

numberedPrimesWrong :: [(Int,Int)]
numberedPrimesWrong = [ (i, p) | p <- primes , i <- [1..] ]

{- The problem of this attempt is that we will try to range through
   all indices in the infinite list [1..] before considering the
   next prime, which understandably fails to achieve what we want:

      *Lec06> take 10 numberedPrimesWrong
      [(1,1),(2,1),(3,1),(4,1),(5,1),(6,1),(7,1),(8,1),(9,1),(10,1)]

   (The function take from the standard library that returns the n
   first elements of a list, by the way. Useful for inspecting
   infinite lists without having to interrupt the output.)

   What we instead want to do is "zip together" the two lists
   by walking down both of them in lock step. Let us ignore the
   fact that this function is already in the standard library,
   and re-implement it using good old pattern matching: -}

zip :: [a] -> [b] -> [(a,b)]
zip []     _      = []
zip _      []     = []
zip (x:xs) (y:ys) = (x,y):(zip xs ys)

{- If we enable the ParallelListComp LANGUAGE extension (like
   we did at the top of this file), we can also write zip
   using a /parallel list comprehension/: -}

zip' :: [a] -> [b] -> [(a,b)]
zip' xs ys = [ (x, y) | x <- xs | y <- ys]

{- Here the first vertical bar means "such that", but the
   second one means "in parallel with".

   Now it is easy to write the definition of the numbered
   primes that we wanted: -}

numberedPrimesProperly :: [(Int, Int)]
numberedPrimesProperly = zip' [1..] primes

{- The zip function, or parallel list comprehensions, can be
   used to write some mindblowing definitions. Recall the famous
   Fibonacci sequence

     1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181,...

   used by the Italian mathematician Leonardo Fibonacci to describe
   the growth of a population of rabbits. (Incidentally, the Indian
   mathematician Virahaṅka studied the Fibonacci sequence 600 years
   before Fibonacci.)

   Here is the definition: -}

fibs :: [Int]
fibs = 1:1:[ x + y | x <- fibs | y <- tail fibs]

{- Again, this works because of lazy evaluation; we provide just
   enough data upfront by the first two entries in the list to produce
   the third entry, which in turn is enough to produce the fourth entry,
   and so on. It can be instructive to step through the reduction of
   fibs with pen and paper, but don't worry if this seems magical at
   this point. -}



{-    Part 4.6 : LIST COMPREHENSIONS FOR A SIMPLE DATABASE

-- FIXME: tidy this up

   One can model databases in a simple manner as tables of key-value
   pairs, and use list comprehensions as a quite readable query
   language. Here is the "query" for looking up all values associated
   with a given key in such a table: -}

lookup :: Eq a => a -> [(a,b)] -> [b]
lookup k t = [ b | (a,b) <- t, a == k ]

{- It can be helpful to compare this with the SQL query

     SELECT b
       FROM t
      WHERE a = k -}

{- Here is a slightly bigger example, adapted from Simon Thompson's
   book 'Haskell: The Craft of Functional Programming'. -}

type Person = String
type Book = String
type Fee  = Integer

{- We model a library database, keeping track of who has,
   borrowed which book, and how much they owe in late fees.
   A database is again just a table of tuples of data. -}

type Database = [ (Person, Book, Fee) ]

exampleDB :: Database
exampleDB = [("Alice", "Tintin", 1)
            ,("Anna","Little Women", 2)
            ,("Alice","Asterix", 5)
            ,("Rory","Tintin", 0)
            ]

{- The following "query" finds all books borrowed by a given
   person: -}

books :: Database -> Person -> [Book]
books db per = [ book | (per', book, _) <- db, per == per' ]

{- Again, compare with

     SELECT book
       FROM db
      WHERE per = per'

   As expected:

      *Lec06> books exampleDB "Alice"
      ["Tintin","Asterix"]

   Note that writing -}

booksWrong :: Database -> Person -> [Book]
booksWrong db per = [ book | (per, book, _) <- db ]

{- does not do what we want: it might look like the second use
   of the same variable name per would force them to be equal, but
   in fact this will just introduce a new variable with a the same
   name, "shadowing" the previous variable. (The same thing happens
   if one e.g. defines a function Int -> Int -> Int by a lambda
   abstraction: writing -}

foo :: Int -> Int -> Int
foo  = \ x -> \ x -> x

{- will not force both arguments to be equal, but the second x will
   silently take precedence over the first, as you can see if you
   for example try to evaluate f 1 2. if you start GHCi with the
   commandline option fwarn-name-shadowing (e.g.
   "ghci -fwarn-name-shadowing lectures/Lec06.hs"), GHC will warn
   you when this happens:

      GHCi, version 8.0.2: http://www.haskell.org/ghc/  :? for help
      [1 of 1] Compiling Lec06            ( lectures/Lec06.hs, interpreted )

      lectures/Lec06.hs:352:31: warning: [-Wname-shadowing]
          This binding for ‘per’ shadows the existing binding
           bound at lectures/Lec06.hs:352:15

      lectures/Lec06.hs:362:17: warning: [-Wname-shadowing]
          This binding for ‘x’ shadows the existing binding
            bound at lectures/Lec06.hs:362:10
      Ok, modules loaded: Lec06.
      *Lec06>

   End of digression. -}

{- This query finds all late books, and their borrowers: -}

lateBooks :: Database -> [(Book,Person)]
lateBooks db = [ (book,per) | (per, book, fee) <- db, fee > 0 ]


{-     PART 2.5: JOINING FILES IN A HACKY WAY

   We can use list comprehensions to join two files in a
   quick-and-dirty way in GHCi. In real life, we do this
   to e.g. combine a register file containing your user names
   and email addresses, and another file containing your user
   names and exercise marks, but in order to avoid the Data
   Protection Act, let's look at an example using public open
   data on births and deaths in Glasgow in 2012 (obviously).
   You can download this and many more data files from

      https://data.glasgow.gov.uk/

   The files in question can be found in the git repository
   in the lectures/Lec06-data subdirectory. Looking at the
   files, we see that death.csv contains both "data zones"
   and "intermediate geography names", whereas birth.csv
   contains a "geography code" (matching the data zone from
   the other file) only. As a first step, we would thus like
   to join the files so that we can see the more human-readable
   name also for the death statistics. In GHCi, we can do this
   as follows:

      *Lec06> readFile "Lec06-data/birth.csv"  -- read the file
      "GeographyCode:CS-allbirths:CS-femalebirths:CS-malebirths\nS01003025:5:3:2\nS01003026:17:7:10\nS01003027:17:9:8\nS01003028:6:5:1\nS01003029:14:5:9\n[...]
      *Lec06> lines it -- convert string into list of lines
      ["GeographyCode:CS-allbirths:CS-femalebirths:CS-malebirths","S01003025:5:3:2","S01003026:17:7:10","S01003027:17:9:8","S01003028:6:5:1","S01003029:14:5:9",[...]]
      *Lec06> map (splitOn ":") it -- split up each line
      [["GeographyCode","CS-allbirths","CS-femalebirths","CS-malebirths"],["S01003025","5","3","2"],["S01003026","17","7","10"],["S01003027","17","9","8"],["S01003028","6","5","1"],["S01003029","14","5","9"],[...]]
      *Lec06> let birth = it

   At each stage, "it" refers to the result of the previous
   computation. By doing things in stages, we don't have to
   remember exactly what to do in what order from the very
   beginning. However when we now do the same for the second
   file, we have spotted the pattern:

      *Lec06> readFile "Lec06-data/death.csv"
      "Data Zone:Intermediate Geography Name:CS-alldeaths\nS01003025:Carmunnock South:13\nS01003026:Carmunnock South:13\nS01003027:Darnley East:23\nS01003028:Glenwood South:11\nS01003029:Carmunnock South:12\nS01003030:Glenwood South:6\nS01003031:Glenwood South:0\nS01003032:Glenwood South:7\nS01003033:Glenwood South:24\nS01003034:Darnley East:1\n[...]
      *Lec06> let death = map (splitOn ":") (lines it)

   We can now join the files using a list comprehension:

      *Lec06> let joined = [ (name ++ " " ++ zone, b, d) | [zone, name, d] <- death, [zone', b, _, _] <- birth, zone == zone' ]

   The following function can be used to print the table in more
   readable form. Don't worry to much about the details of it for
   now.
-}

{-printTable :: [(String,String,String)] -> IO ()
printTable  = mapM_ (\ (n,b,d) -> putStrLn $ n ++ ":" ++
                                  (replicate (51 - length n) ' ') ++
                                  b ++ "    \t" ++ d) . sort
-}
{-

      *Lec06> printTable joined
      "Calton, Galllowgate and Bridgeton" S01003248:      22    13
      "Calton, Galllowgate and Bridgeton" S01003270:      14    8
      "Calton, Galllowgate and Bridgeton" S01003271:      20    11
      "Calton, Galllowgate and Bridgeton" S01003328:      7     3
      "Calton, Galllowgate and Bridgeton" S01003331:      10    11
      "Calton, Galllowgate and Bridgeton" S01003333:      9     18
      "Calton, Galllowgate and Bridgeton" S01003335:      6     5
      "Cranhill, Lightburn and Queenslie Sout" S01003372: 9     20
      "Cranhill, Lightburn and Queenslie Sout" S01003377: 8     9
      "Cranhill, Lightburn and Queenslie Sout" S01003383: 6     16
      "Cranhill, Lightburn and Queenslie Sout" S01003401: 14    5
      "Cranhill, Lightburn and Queenslie Sout" S01003404: 5     10
      "Cranhill, Lightburn and Queenslie Sout" S01003413: 8     6
      "Cranhill, Lightburn and Queenslie Sout" S01003421: 11    10
      "Cranhill, Lightburn and Queenslie Sout" S01003428: 25    7
      "Garthamlock, Auchinlea and Gartloch" S01003444:    10    9
      "Garthamlock, Auchinlea and Gartloch" S01003462:    10    6
      "Garthamlock, Auchinlea and Gartloch" S01003476:    29    2
      "Garthamlock, Auchinlea and Gartloch" S01003502:    52    1
      "Garthamlock, Auchinlea and Gartloch" S01003528:    7     1
      "Roystonhill, Blochairn, and Provanmill" S01003442: 20    11
      "Roystonhill, Blochairn, and Provanmill" S01003443: 15    6
      "Roystonhill, Blochairn, and Provanmill" S01003445: 21    6
      "Roystonhill, Blochairn, and Provanmill" S01003457: 29    12
      "Roystonhill, Blochairn, and Provanmill" S01003458: 18    10
      "Roystonhill, Blochairn, and Provanmill" S01003488: 9     25
      Anderston S01003382:                                25    13
      Anderston S01003408:                                8     9
      Anderston S01003423:                                8     2
      Anderston S01003430:                                8     1
      Anniesland East S01003612:                          7     25
      Anniesland East S01003632:                          9     2
      Anniesland East S01003635:                          1     8
      Anniesland East S01003661:                          16    4
      Anniesland East S01003675:                          8     4
      [...]

   We can run further queries, for instance we can see in how
   many areas the population decreased, increased or stayed
   the same in 2012:

      *Lec06> length [ (name ++ " " ++ zone, b, d) | [zone, name, d] <- death, [zone', b, _, _] <- birth, zone == zone', (read b :: Int) < read d ]
      265
      *Lec06> length [ (name ++ " " ++ zone, b, d) | [zone, name, d] <- death, [zone', b, _, _] <- birth, zone == zone', (read b :: Int) > read d ]
      385
      *Lec06> length [ (name ++ " " ++ zone, b, d) | [zone, name, d] <- death, [zone', b, _, _] <- birth, zone == zone', (read b :: Int) == read d ]
      44

   (Here we are using the function read from the Read type class
   to convert a string into an Int.) -}
