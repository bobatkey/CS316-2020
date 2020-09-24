{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week02 where

{- CS316 2020/21 : Week 2

             SOLVING PROBLEMS BY RECURSION

   This week, we continue our exploration of solving problems in
   Haskell by the use of recursion, and look at other ways to make
   decisions beyond pattern matching. FIXME
-}


{-     Part 2.1 : SEARCHING LISTS

   Let's try to solve the following simple problem: to search through
   a list and find out whether or not some value is in it. If we
   assume for now that we're going to just be searching lists of
   'Integer's, then we have the following type for our function: -}

isMember0 :: Integer -> [Integer] -> Bool

{- The empty list case returns False: there is nothing in the empty
   list. -}

isMember0 x [] = False

{- In the 'cons' case, we need to compare the head of the list with the
   thing we are looking for.

   We can't only use pattern matching to solve this problem, because
   there is no way of writing a pattern to say "the same as the value
   from that other pattern". If we try something like:

     isMember0 x (x:xs) = True

   then GHC complains with:

        lecture-notes/Week02.hs:39:11-14: error: …
            • Conflicting definitions for ‘x’
              ...

   So we need to match the thing we are searching for ('x') and the
   thing we are looking at ('y') with two different names. We then use
   an if-then-else expression with an equality test to compare 'x' and
   'y': -}

isMember0 x (y:ys) = if x == y then True else isMember0 x ys

{- if-then-else has the following structure:

     if <condition> then <result-if-True> else <result-if-False>

   So in this case, if 'x == y', then it the result is 'True',
   otherwise the result is 'isMember0 x ys', meaning that it searches
   the rest of the list.

   We can test this function to make sure it works:

      *Week02> isMember0 1 [1,2,3]
      True
      *Week02> isMember0 0 [1,2,3]
      False

   What if we want to search through lists of things other than
   'Integer's? One tempting thing to do is to replace all the
   'Integer's in the type signature with 'a's to stand for any type.

   This doesn't work (try it!) because not all types support a way to
   compare their values for equality. To restrict ourselves to types
   that have an equality test, we need to add a *constraint* to the
   type signature. In this case, the constraint 'Eq a' says that any
   type 'a' we use this function with must support an equality
   test. 'Eq' is a typeclass (i.e. a class of types sharing common
   structure). We will come back to type classes in Week 05, but for
   now think of them as a bit like interfaces in Java. The constraint
   'Eq a' is something like "the type 'a' must implement the 'Eq'
   interface.

   Generalising 'Integer' to 'a' and add an 'Eq' constraint means that
   we can write exactly the same code, and have it work for any type
   that supports equality: -}

isMember :: Eq a => a -> [a] -> Bool
isMember y []     = False
isMember y (x:xs) = if x == y then True else isMember y xs

{- For example,

     *Week02> isMember "a" ["a","b"]
     True
     *Week02> isMember "c" ["a","b"]
     False

   (What happens if you try to search for an integer in a list of
   strings?)

   Using if-then-else is not the only way to make decisions in
   Haskell. Another way is to use guards. Here is an example: -}

isMember1 :: Eq a => a -> [a] -> Bool
isMember1 x [] = False
isMember1 x (y:ys)
  | x == y    = True
  | otherwise = isMember1 x ys

{- Guards have the form

   functionName <patterns>
     | <condition1> = <result1>
     | <condition2> = <result2>
     ...
     | <conditionN> = <resultN>

   Once the <patterns> have been matched, Haskell evaluates each of
   the <condition>s in order. The first one to match activates that
   line. If none match, an exception is raised. To avoid exceptions,
   usually the last guard has the condition 'otherwise' which
   evaluates to 'True' and so always matches.

   Guards are useful for avoiding long chains of 'if-then-else', as we
   shall see below.

   Another way of writing this function is to use boolean
   operators. Thinking that "'y' is in the list if either it is the
   head or it is in the tail", then we can write 'isMember' using the
   boolean or operator '||': -}

isMember2 :: Eq a => a -> [a] -> Bool
isMember2 x []     = False
isMember2 x (y:ys) = x == y || isMember2 x ys

{- Finally in this part, what if we assume that the input list is sorted?

   If we know that the list is sorted in ascending order, then we can
   stop searching if we find an element larger than the one we are
   looking for. We can implement this idea using 'guards': -}

isMember3 :: Ord a => a -> [a] -> Bool
isMember3 x [] = False
isMember3 x (y:ys)
  | x == y    = True
  | x < y     = False
  | otherwise = isMember3 x ys

{- For this function, we've had to have the constraint 'Ord', stating
   that it only works for types are that are 'Ord'ered. This allows us
   to use the '<' comparison. -}

-- QUESTION: counting the number of occurrences of an element


{-     PART 2.2 : REMOVING AND INSERTING ELEMENTS

   We've seen how to search sorted lists, how about adding and
   removing elements from them?
-}

{- Consider the problem of inserting a value into a sorted list, so
   that the resulting list is still sorted. If we assume that the
   input list is sorted in ascending order, then there are three cases
   to consider:

      1) insertion into the empty list -- we return a list with one
         element

      2) insertion into a list when the head is greater than the
         element to be inserted -- we return the new element as the
         head of the result, with the input list as the tail.

      3) insertion into a list when the head is less than the element
         to be inserted -- we return the head followed by the
         insertion of the element into the rest of the list.

   We can write this function using a mixture of pattern matching to
   look at the structure of the list and 'guard's to do the
   comparisons: -}

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x < y     = x : y : ys
  | otherwise = y : insert x ys

{- We can see how 'insert' operates by writing out a trace of how it
   works on an example list:

           insert 3 [1,4]
         =
           1 : insert 3 [4]
         =
           1 : 3 : 4 : []
         =
           [1,3,4]
-}

remove :: Ord a => a -> [a] -> [a]
remove y [] = []
remove y (x:xs)
  | x == y    = xs
  | x < y     = x:remove y xs
  | otherwise = x:xs



{- FIXME: The functions we have written in this part are structurally
   recursive: when we call 'insert' inside the definition of 'insert',
   we are using a value ('ys') we got from the input. Therefore, we
   can say that 'insert' follows the structure of its input. -}

-- QUESTION: insertion without duplication
-- QUESTION: removal of all duplicates

{-    PART 2.3 : INSERTION SORT AND QUICKSORT

   Using 'insert', we can write a sorting function by repeatedly
   inserting each element into a sorted list. Again, we can define
   this function by structural recursion on the input list: -}

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)

{- The advantage of using structural recursion is that it is easier to
   reason that 'isort' always produces sorted lists which have the
   same elements as the input:

     - when the input is [], we return [], which is sorted.

     - when the input is 'x:xs', we sort 'xs', and then insert 'x'
       into the result. Since insertion into a sorted list always
       gives us a sorted list, we know that the overall result is
       sorted. Also, 'insert' inserts the element exactly once, so we
       know that the result has the same elements as the input.

   However, 'isort' has a problem, which we can see by writing out the
   trace of sorting a reversed list and writing the number of steps
   each time:

        isort [3,2,1]
       =   { 4 }
        insert 3 (insert 2 (insert 1 []))
       =   { 1 }
        insert 3 (insert 2 [1])
       =   { 2 }
        insert 3 [1,2]
       =   { 3 }
        [1,2,3]

   From this, we can see that 'isort' effectively transforms its input
   into a list of 'insert' jobs. Because the initial list was in
   reverse order, each 'insert' job has to go right to the end to
   insert its element. This means that, in the worst case, we take a
   number of steps proportional to the square of the input list to
   accomplish the sort. Can we do better?

   An algorithm for sorting that is, sometimes, faster than insertion
   sort is Hoare's QuickSort algorithm. QuickSort works by dividing
   the input into two large chunks and then sorting those
   independently. Therefore, it can be more efficient than insertion
   sort, which always splits the input into one very small chunk (the
   head) and the rest.

   We can write a short implementation of a simple version of
   QuickSort in Haskell. As above, sorting the empty list yields the
   empty list. To sort a list with an element 'x', we split it into
   two lists: 'smaller', which contains everything less than 'x', and
   'larger', which contains everything greater than or equal to 'x',
   then we sort those lists and stick everything back together using
   the built-in append function '++': -}

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = [ y | y <- xs, y < x ]
        larger  = [ y | y <- xs, y >= x ]

-- FIXME: remove the list comprehensions

{- We have used two new constructs here:

   1. 'where' allows us to split out parts of a definition and write
      them separately. We could have written the second case of
      'qsort' as:

          qsort (x:xs) = qsort [ y | y <- xs, y < x] ++ [x] ++ qsort [ y | y <- xs, y >= x]

      instead of naming the two lists 'smaller' and 'larger'. However,
      using 'where' allows us to be clearer about why we are doing
      certain things by giving them names. (It is also more efficient
      if we use the same thing more that once.)

   2. The [ y | y <- xs, y < x ] is a list comprehension. We can read
      it as "make a list of all the 'y's in 'xs' such that 'y' is less
      than 'x'". We will come back to list comprehensions in Week 04.

   ASIDE: Unfortunately, this isn't a very good implementation of
   QuickSort, and some might say it is not really QuickSort at
   all. QuickSort, as originally defined by Hoare, operated on arrays
   and sorted in instead of creating (a lot of) new lists as this
   implementation does. For more informaton / opinions, see:

      https://stackoverflow.com/questions/7717691/why-is-the-minimalist-example-haskell-quicksort-not-a-true-quicksort

   Also the way that the pivot element is selected in this
   implementation is very naive, and can often yield the same worst
   case time behaviour as insertion sort. Nevertheless, it is a good
   example of a non-structurally recursive function for our purposes.

   END OF ASIDE.

   The definition of 'qsort' is all very well, but it is not
   structurally recursive. We call 'qsort' recursively on lists that
   are computed via a (relatively) complex list comprehension, and not
   just ones that are discovered by pattern matching. This makes it
   harder to see that 'qsort' is definitely doing the right
   thing.

   To help us see what is going on inside 'qsort', let's step through
   an example:


     qsort [5,3,1,2]
   =
     qsort [3,1,2]                                               ++ [5] ++ qsort []
   =
     (qsort [1,2]                            ++ [3] ++ qsort []) ++ [5] ++ []
   =
     (qsort [1,2]                            ++ [3] ++ qsort []) ++ [5] ++ []
   =
     ((qsort [] ++ [1] ++ qsort [2])         ++ [3] ++ [])       ++ [5] ++ []
   =
     (([]       ++ [1] ++ ([] ++ [2] ++ [])) ++ [3] ++ [])       ++ [5] ++ []
   =
     [1,2,3,5]

   We have formatted this example to reveal some of the internal
   structure of the tasks that 'qsort' generates. Looking at the final
   structure of the appends ('++'s) at the end, we can see that there
   is a tree structure:

                       [5]
                  [3]      []
              [1]    []
            []  [2]
               [] []

   Let's now see how to reformulate 'qsort' in terms of intermediate
   tree data structure, which will help us make a structurally
   recursive variant. -}


{-   PART 2.3 : TREESORT

   We want to represent binary trees, so we create a new data type for
   this purpose. We name this data type 'BST' for Binary Search Tree
   to indicate that we want it to have a special property with respect
   to sortedness. Specifically, a tree is a binary search tree if:

       1. it is 'Leaf'; or

       2. it is 'Node l x r' and all of the following are true:
          (a) every value in l is <= x
          (b) every value in r is >= x
          (c) l is a binary search tree
          (d) r is a binary search tree
-}

data BST a
  = Leaf
  | Node (BST a) a (BST a)
  deriving Show

{- We will build up our 'BST's by inserting elements into them,
   maintaining the properties listed above. This insertion function is
   analogous to the 'insert' function on lists we defined above. As
   above, there are three cases:

       1. The tree is empty: we make a new tree with a single node;

       2. The element at the root of the tree is less than the element
          we want to insert: we insert the element into the left hand
          (smaller) subtree;

       3. The element at the root of the tree is greater than or equal
          to the element we want to insert: we insert the element into
          the right hand (larger) subtree.

   We write out these cases using pattern matching and guards. As with
   the 'insert' function above, this function is structurally
   recursive and we can check for each case that it (a) always returns
   a Binary Search Tree; and (b) the values in the result tree are all
   the values in the input tree, plus the new value. -}

insertBST :: Ord a => a -> BST a -> BST a
insertBST x Leaf = Node Leaf x Leaf
insertBST x (Node smaller y larger)
  | x < y     = Node (insertBST x smaller) y larger
  | otherwise = Node smaller y (insertBST x larger)

{- As we saw above, 'qsort' operates by converting the input list into a
   tree of jobs to perform. We copy this idea by writing a (structurally
   recursive) function to convert a list to a tree by repeated
   insertion: -}

listToTree :: Ord a => [a] -> BST a
listToTree []     = Leaf
listToTree (x:xs) = insertBST x (listToTree xs)

{- We can see how this generates the same trees as qsort (after
   reversal, because it builds up the tree from the last element to
   the first):

     > listToTree (reverse [5,3,1,2])
     Node (Node (Node Leaf 1 (Node Leaf 2 Leaf)) 3 Leaf) 5 Leaf

   This is exactly the Haskell representation of the qsort tree we
   drew above.

   Now, to convert a tree to a list, we 'flatten' it. You already saw
   this function in Exercise 1. We work on the structure of the tree,
   converting leaves to empty lists, and converting nodes to the
   concatenation of the smaller, middle bit, and larger parts: -}

flatten :: BST a -> [a]
flatten Leaf = []
flatten (Node smaller a larger) =
  flatten smaller ++ [a] ++ flatten larger

{- Finally, we can put 'flatten' and 'listToTree' together to get the
   'treesort' function: -}

treesort :: Ord a => [a] -> [a]
treesort xs = flatten (listToTree xs)



{-   PART 2.5 : SEARCHING AND UPDATING TREES


-}


treeMember :: Ord a => a -> BST a -> Bool
treeMember a Leaf = False
treeMember a (Node l x r)
 | a == x    = True
 | a < x     = treeMember a l
 | otherwise = treeMember a r




{-   PART 6 : BACKTRACKING SEARCH -}

type Coin = Int

orElse :: Maybe a -> Maybe a -> Maybe a
orElse Nothing  x = x
orElse (Just a) _ = Just a

makeChange :: [Coin] -> [Coin] -> Int -> Maybe [Coin]
makeChange coins        used 0 = Just used
makeChange []           used _ = Nothing
makeChange (coin:coins) used amount
  | amount >= coin =
    makeChange coins (coin:used) (amount - coin)
    `orElse`
    makeChange coins used amount
  | otherwise =
    makeChange coins used amount

-- TODO: makeChange with lists

-- Question: makeChange with replacement
