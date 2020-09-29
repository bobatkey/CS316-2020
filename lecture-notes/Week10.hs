{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week10 where

{-   WEEK 10 : INFINITE DATA AND PROCESSES -}

-- Lec17

{-    PART 1 : HOW HASKELL EVALUATES -}

{-    PART 2 : TERMINATION BEHAVIOUR -}

{-    PART 3 : SHARING AND LAZINESS -}

{-    PART 4 : LAZINESS, PROCRASTINATION AND STRICTNESS -}

{-    PART 5 : INFINITE DATA -}




{- An example of a longer pipeline is the following, which selects every
   other element from a list: -}

everyOther :: [a] -> [a]
everyOther = map snd . filter (\ (i,x) -> i `mod` 2 == 1) . zip [0..]

{- How does this work? Let's break it down:

     1. First, we pair every element in the input list with its index
        by zipping with the infinite list [0..] (remember how we did
        this in Lecture 03)

     2. Then we filter to get only the element of the list with odd
        index.

     3. Then we map 'snd' over the list to throw away the indexes and
        keep the data.

   Graphically, we can visualise the pipeline as follows, with types
   for the intermediate stages:

       zip               filter (...)              map snd
   [a] ---> [(Int, a)] ----------------> [(Int,a)] --------> [a]

   Unfortunately, 'everyOther' isn't particularly efficient. In any
   list of reasonable size, we'll be generating quite large numbers
   when all we are really interested in is whether or not they are
   odd.

   An alternative strategy is to zip with the infinite list

       [False, True, False, True, False, True, ...]

   This will produce a list like:

       [(False, x1), (True, x2), (False, x3), (True, x4), ...]

   Keeping the elements with 'True' in the first element will yield:

       [(True, x2), (True, x4), (True, x6), ...]

   And mapping 'snd' will give us:

       [x2, x4, x6, ...]

   as we want.

   Happily, the Haskell standard library function 'cycle' can produce
   infinite lists like [False, True, False, True, ...]. Given any
   finite list 'l', 'cycle l' repeats that list over and over again
   forever. We can use 'cycle' to code up this alternative strategy
   for 'everyOther': -}

everyOther2 :: [a] -> [a]
everyOther2 =  map snd . filter fst . zip (cycle [False, True])
