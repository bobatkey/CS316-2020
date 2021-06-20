module Ex2Resit where

{----------------------------------------------------------------------}
{- CS316 (2020/21 RESIT) EXERCISE 2 : HIGHER-ORDER PROGRAMMING        -}
{----------------------------------------------------------------------}

{- The resit questions consist of:

     1. Exercise 1 (30 marks)
     2. Exercise 2 (30 marks) (this one)
     3. Exercise 3 (40 marks)

   To pass, you need to get over 40%.

   Submit by emailing your answers for this exercise and the other two
   to the course lecturer

     Robert Atkey <robert.atkey@strath.ac.uk>

   by Thursday 22nd July 2021 17:00. We will then arrange a short Zoom
   meeting for you to present your solutions.

   Note about plagiarism: For the take home parts of this exercise,
   you can discuss the questions with others to make sure that you
   understand the questions. However, you must write up your answers
   by yourself. Plagiarism will be taken very seriously. -}

{----------------------------------------------------------------------}
{- PART 1 : HIGHER ORDER FUNCTIONS ON TREES                           -}
{----------------------------------------------------------------------}

{- Here is the 'Tree' datatype again. -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- As we saw in Week 4, it is possible to write a generic recursor
   pattern for trees, similar to 'foldr': -}

foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
foldTree l n Leaf           = l
foldTree l n (Node lt x rt) = n (foldTree l n lt) x (foldTree l n rt)


{- 2.1.1 Multiply Tree

   Use 'foldTree' to write a function that multiplies together all the
   numbers stored in a 'Tree' of 'Int's.

   For example:

      mulTree (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)) == 6

   (because 1 * 2 * 3 == 6)
-}

mulTree :: Tree Int -> Int
mulTree = undefined

{- 2 MARKS -}

{- 2.1.2 Flatten Tree

   Use 'foldTree' to write a function that concatenates all the lists
   contained in a 'Tree' of lists (so it "flattens" the tree).

   For example:

     flattenTree (Node (Node Leaf [1,2] Leaf) [3] (Node Leaf [1,2] Leaf)) == [1,2,3,1,2]
-}

flattenTree :: Tree [a] -> [a]
flattenTree = undefined

{- 2 MARKS -}

{- 2.1.2 Implement 'mapTree' in terms of 'foldTree'.

   Use 'foldTree' to implement a function that acts the same as the
   'mapTree' function from Week 3. -}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined

{- Here is the explicitly recursive version of 'mapTree', for
   reference: -}

mapTree0 :: (a -> b) -> Tree a -> Tree b
mapTree0 f Leaf           = Leaf
mapTree0 f (Node lt x rt) = Node (mapTree0 f lt) (f x) (mapTree0 f rt)

{- 2 MARKS -}

{----------------------------------------------------------------------}
{- PART 2 : COMPARISON OPERATORS                                      -}
{----------------------------------------------------------------------}

{- The Haskell standard library predefines a type 'Ordering' for
   describing ordering relationships between values:

      > :info Ordering
      data Ordering = LT | EQ | GT
      [...]

   This type is used by the 'Ord' type class. We used the 'Ord' type
   class in Week 2 to write sorting functions. If a type is a member
   of the Ord type class we can compare values of that type. This is
   what allows us to use '<' and '>='. The Ord type class also defines
   a function 'compare':

      > :info Ord
      class Eq a => Ord a where
        compare :: a -> a -> Ordering
        [...]

   So 'compare' returns the appropriate result for its two
   arguments. For example:

         > compare 1 2
         LT
         > compare 2 1
         GT
         > compare 1 1
         EQ

   Sometimes, the default ordering for a type is not the one we
   want. For example, we might want to sort the list into descending
   order instead of ascending order. Below, we will write sorting
   functions that take the ordering to use as an explicit argument.

   To do this, we need to isolate the idea of a thing that compares
   two values of the same type. We will use first class functions to
   do this.

   A 'Comparator' in Haskell is a function that takes two values and
   returns an 'Ordering' (satisfying some properties). Let's make a
   type synonym for Comparators: -}

type Comparator a = a -> a -> Ordering

{- Every type that is a member of the 'Ord' type class has a default
   comparator. We just write 'compare' for this, and Haskell's type
   inference mechanism will work out which one to use.

   However, the default comparator might not be the ordering that we
   wish to use. So we will build new ones.

   We won't always build comparators from scratch though, we'll see
   how to build new comparators out of old ones.

   (ASIDE: To be a proper comparator, we ought to also have some properties
    for any comparator 'cmp':

      1. cmp x y == invertOrdering (cmp y x)
      2. if (cmp x y == LT) and (cmp y z == LT) then (cmp x z == LT)
      3. if (cmp x y == EQ) then, for all z, (cmp x z == cmp y z)

    We won't get into worrying about these for this exercise
    though.) -}

{- 2.2.0 Inverting Comparators.

   We can invert an 'Ordering': -}

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT

{- Write a function that takes as input a comparator and returns a
   comparator that implements the reverse ordering. Use
   'invertOrdering'. -}

invert :: Comparator a         -> Comparator a
       -- (a -> a -> Ordering) -> (a -> a -> Ordering)
invert = undefined

{- For example:

        > invert compare 1 2
        GT
        > invert compare 2 1
        LT
        > invert compare 1 1
        EQ
-}

{- 1 MARK -}


{- 2.2.1 Transforming Comparators.

   If we have a 'Comparator a' and a way of turning 'b's into 'a's, we
   can build a 'Comparator b'. Implement this: -}

on :: Comparator a -> (b -> a) -> Comparator b
on = undefined

{- For example, to compare pairs on their first element, we might write:

       compare `on` fst :: Ord a => Comparator (a,b)

   Or to compare lists by their length:

       compare `on` length :: Comparator [a]
-}

{- 2 MARKS -}


{- 2.2.2 Sorting with a comparator.

   In the lectures, we have seen insertion sort and quicksort. Both of
   these have the problem that their worst case execution time is
   quadratic. A sorting algorithm that is always O(n log n) is merge
   sort. This works by splitting the input list into two by putting
   the even elements in one list, the odd elements in the other list,
   recursively sorting the two lists, and then merging the results
   maintaining the order.

   Here is a Haskell implementation of merge sort. 'mergeSort' handles
   the main recursion described above, 'split' does the splitting, and
   'merge' does the merging.

   Try the component functions on a few examples to get a feel for how
   they work. Note that 'merge' only works as expected when the inputs
   are already sorted. -}

mergeSort :: Ord a => [a] -> [a]
mergeSort []  = []
mergeSort [x] = [x]
mergeSort xs  = merge (mergeSort xs1) (mergeSort xs2)
  where (xs1, xs2) = split xs [] []

split :: [a] -> [a] -> [a] -> ([a],[a])
split []       ys zs = (ys, zs)
split [x]      ys zs = (x:ys, zs)
split (y:z:xs) ys zs = split xs (y:ys) (z:zs)

merge :: Ord a => [a] -> [a] -> [a]
merge []     ys     = ys
merge xs     []     = xs
merge (x:xs) (y:ys) = if x < y then x : merge xs (y:ys) else y : merge (x:xs) ys

{- 'mergeSort' as written above relies on the implementation of 'Ord'
   for the type 'a' to do the comparisons. Rewrite 'mergeSort' so that
   it takes as input a 'Comparator a', instead of relying on the
   default one from the 'Ord' instance. You will need to also write a
   new definition of 'merge', called 'mergeWith'. -}

mergeSortWith :: Comparator a -> [a] -> [a]
mergeSortWith cmp = undefined

mergeWith :: Comparator a -> [a] -> [a] -> [a]
mergeWith cmp = undefined


{- It should be the case that 'mergeSortWith compare' always gives the
   same answer as 'mergeSort'. For example:

      > mergeSortWith compare [5,2,3,4,1]
      [1,2,3,4,5]
      > mergeSortWith compare ["c", "aaa", "bb"]
      ["aaa","bb","c"]

   But when we use the functions above, we get different orderings:

      > mergeSortWith (invert compare) [5,2,3,4,1]
      [5,4,3,2,1]
      > mergeSortWith (compare `on` length) ["c", "aaa", "bb"]
      ["c","bb","aaa"]
-}

{- 5 MARKS -}


{- 2.2.3 Using 'mergeSortWith' and the functions above, write a function
   that sorts lists of '(Int,String)' in reverse order on the length
   of the second element of each pair.

   You should use the 'on' function, don't write the comparison by
   hand. -}

sortOnReverseSndLength :: [(Int,String)] -> [(Int,String)]
sortOnReverseSndLength = undefined

{- Example:

     > sortOnReverseSndLength [(1,"one"), (2,"two"), (3,"three"), (4,"four"), (5, "five"), (6, "six")]
     [(3,"three"),(4,"four"),(5,"five"),(6,"six"),(2,"two"),(1,"one")]
-}

{- 2 MARKS -}

{----------------------------------------------------------------------}
{- PART 3 : PROCESSES                                                 -}
{----------------------------------------------------------------------}

{- This part of the exercise generalises the communicating processes
   from Exercise 1 to allow processes that send and recieve data of
   any type, not just booleans. These processes are also a kind of
   tree, except that now the choices after an input are represented by
   a function, instead of a branch for 'True' and a branch for
   'False'. These processes can also return a final value.

   I'll set things up, then it'll be your turn.

   'Process x a' is the type of processes that send and recieve values
   of type 'x' and terminate with a value of type 'a'.

   For example, we could think of simplified Unix processes that can
   only talk to Standard Input and Standard Output as values of type
   'Process Word8 Int'. They send and recieve 8-bit bytes
   (i.e. 'char's) and terminate with an int-value exit status. -}
data Process x a
  = End a -- marks the end of a process, returning a value of type
          -- 'a'.
  | Input (x -> Process x a) -- (Input k) requests input of a value
                             -- 'v' of type 'x', and chooses a
                             -- continuation process (k v) based on
                             -- that value.
  | Output x (Process x a) -- (Output v k) outputs a value 'v' of type
                           -- 'x' and continues as the process 'k'.

{- Let's have some example processes. First, the notGate example from
   Exercise 1, rewritten to be a member of the more general 'Process'
   type: -}

notGate :: Process Bool ()
notGate = Input (\b -> Output (not b) (End ()))

{- See how this is the same as the 'notGate' example in Exercise 1, only
   here instead of explicitly giving the two different options for the
   two possible inputs, we give a function that decides what to do
   instead. In this case, it outputs the 'not' of whatever the input
   is. Using functions instead of explicitly enumerating the cases
   leads to significantly smaller descriptions of processes in most
   cases. -}

{- Let's have another example process: this process inputs any value,
   and then outputs that same value. Note that this process is
   polymorphic in the type 'x' of values it inputs and outputs. -}

echo :: Process x ()
echo = Input (\v -> Output v (End ()))

{- We make processes 'go' in the same way as we did before. We interpret
   them, feeding the 'Input's from a list of inputs, and placing the
   'Output's into a list. There are two main differences with
   'process' from Exercise 1: we need to return the extra value
   attached to 'End', and we explicitly signal lack of input by using
   a 'Maybe' type. -}

process :: Process x a -> [x] -> (Maybe a,[x])
process (End a)      inputs     = (Just a, [])
process (Input k)    []         = (Nothing, [])
process (Input k)    (v:inputs) = process (k v) inputs
process (Output v k) inputs     = (a,v:outputs)
  where (a,outputs) = process k inputs

{- For example,

      process echo ["Hello"] == (Just (),["Hello"])
-}


{- If we have a process that communicates using 'String's, then we can
   make it actually interact with the user using 'runIO'. This
   function translates process descriptions into I/O commands. This
   function uses Haskell's basic facilites for doing real I/O. We will
   come back to this later in the course. -}

runIO :: Process String a -> IO a
runIO (End a)      = return a
runIO (Input k)    = do { s <- getLine; runIO (k s) }
runIO (Output x k) = do { putStrLn x; runIO k }


{- Here's an example of using 'runIO'. The '>' is the haskell prompt.

        > runIO echo
        hello             -- typed by the user
        hello

   where the first 'hello' is typed by the user, and the second is
   printed by the computer. You can use runIO to test your processes
   below, interactively. -}

{- Let's make some basic processes that we can use to build larger
   processes. Your job is to write these from their specifications. -}


{- 2.3.0 'input'.

   'input' is the process that inputs a single value and then ends
   with that value. Write it.

      > runIO input
      foop                 -- typed by the user
      "foop"
-}

input :: Process x x
input = undefined

{- 1 MARK -}


{- 2.3.1 'output'.

   'output x' is the process that outputs the value x, and then ends
   with the value ().

      > runIO (output "Hello!")
      Hello!
-}

output :: x -> Process x ()
output = undefined

{- 1 MARK -}

{- 2.3.2 Greeter.

   Write a process definition that greets the user by asking their
   name:

     > runIO greeter
     Hello, what is your name?
     Haskell
     Hello Haskell!
-}

greeter :: Process String ()
greeter = undefined

{- 4 MARKS -}

{- 2.3.2 May end with

   A process may end with a value 'x' if there is any sequence of
   inputs that results in the final value being returned is equal to
   'x'. Because we have descriptions of processes, we can "look ahead"
   to see whether or not a process will end with 'x':

   - 'End y' may end with 'x' if 'x == y'
   - 'Output a p' may end with 'x' if 'p' may end with 'x'
   - 'Input k' may end with 'x' if 'k True' ends with 'x' or 'k False' ends with 'x'

   Write a function 'mayEndWith' that takes a value 'x' and a process
   'p' and returns 'True' if 'p' may end with 'x' and 'False'
   otherwise. -}

mayEndWith :: Eq a => a -> Process Bool a -> Bool
mayEndWith = undefined

{- 4 MARKS -}

{- 2.3.3 May end with predicate

   Generalise the previous function to test whether a process may end
   with a value matching a given predicate function. -}

mayEndWithPredicate :: (a -> Bool) -> Process Bool a -> Bool
mayEndWithPredicate = undefined

{- 4 MARKS -}

{----------------------------------------------------------------------}
{- END OF EXERCISE                                                    -}
{----------------------------------------------------------------------}
