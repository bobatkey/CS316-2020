{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week02 where

{- CS316 2020/21 : Week 2

             SOLVING PROBLEMS BY RECURSION

   This week, we continue our exploration of solving problems in
   Haskell by the use of recursion, and look at other ways to make
   decisions beyond pattern matching. -}


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



{-     Part 2.2 : REMOVING AND INSERTING ELEMENTS

   We've seen how to search sorted lists, how about adding and
   removing elements from them?

   Consider the problem of inserting a value into a sorted list, so
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
         =                       { 3 >= 1, so we put '1' in the output list }
           1 : insert 3 [4]
         =                       { 3 < 4, so we put '3' in the output list, followed by the remainder of the list }
           1 : 3 : 4 : []
         =                       { the [ , , ] notation is sugar for the ':' notation }
           [1,3,4]
-}

{- Removing elements from a list involves a similar process of searching
   through a list rebuilding it in the output as we go. The difference
   from insertion is that once we have found the element, we remove it
   by *not* returning it in the result.

   Let's see how this works in Haskell: -}

remove :: Ord a => a -> [a] -> [a]
remove y [] = []
remove y (x:xs)
  | x == y    = xs
  | x < y     = x:remove y xs
  | otherwise = x:xs

{- Reading the lines in order:

   - To remove 'y' from the empty list, the result is the empty list.

   - To remove 'y' from a list with head 'x' and tail 'xs':

     - If 'x == y', then we do *not* return 'x' in the output list,
       but we do return the rest of the input 'xs'.

     - If 'x < y', then we know that 'x' may be later in 'xs', so we
       return 'x' in the output, followed by the result of removing
       'y' from 'xs'.

     - Otherwise (when 'x > y'), we know that 'y' cannot appear later
       in the list, so we return the list 'x:xs'.

   It is also worth working out what happens with these functions by
   spelling out by hand the steps involved in removing '2' from the
   list '[1,2,2,3]'. -}



{-    Part 2.3 : INSERTION SORT AND QUICKSORT

   Using 'insert', we can write a sorting function by repeatedly
   inserting each element into a sorted list. Again, we can define
   this function by recursion on the input list: -}

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
   taken each time:

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
   implementation does. For more information / opinions, see:

      https://stackoverflow.com/questions/7717691/why-is-the-minimalist-example-haskell-quicksort-not-a-true-quicksort

   Also the way that the pivot element is selected in this
   implementation is very naive, and can often yield the same worst
   case time behaviour as insertion sort. Nevertheless, it is a good
   example of a non-structurally recursive function for our purposes.

   END OF ASIDE.

   The definition of 'qsort' is all very well, but it is not recursive
   on the *structure* of the input list, like most of the functions we
   have seen so far.

   We call 'qsort' recursively on lists that are computed via a
   (relatively) complex list comprehension, and not just ones that are
   discovered by pattern matching. This makes it harder to see that
   'qsort' is definitely doing the right thing.

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
   tree data structure, which will help us make a variant of quicksort
   that exposes the hidden recursive structure. -}


{-    Part 2.4 : TREESORT

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



{-    Part 2.5 : SEARCHING AND UPDATING TREES

   Binary search trees are useful for many applications beyond
   TreeSort. In this part, we will look at how to use them to
   implement a simple kind of key/value store.

   As a warm up before introducing key/value stores, let's implement
   the equivalent of the 'isMember' function from Part 2.1 for Binary
   Search Trees.

   We will need the constraint 'Ord' on the type of values stored in
   the tree: -}
treeMember :: Ord a => a -> BST a -> Bool

{- The definition now splits depending on whether we are searching a
   'Leaf' or a 'Node' of a tree. 'Leaf's do not store any values, so
   'treeMember' always returns 'False': -}
treeMember x Leaf = False

{- When we are searching a 'Node', there are three sub-cases: either the
   thing we are looking for is equal to the value at the current node,
   or it is less than the current node, or it is greater than the
   current node. In these cases, we return 'True', or search the left
   or right subtree as appropriate: -}
treeMember x (Node l y r)
 | x == y    = True
 | x < y     = treeMember x l
 | otherwise = treeMember x r

{- Let's check this works:

       *Week02> treeMember 4 Leaf
       False
       *Week02> treeMember 4 (Node Leaf 4 Leaf)
       True
       *Week02> treeMember 4 (Node (Node Leaf 4 Leaf) 6 (Node Leaf 7 Leaf))
       True
       *Week02> treeMember 3 (Node (Node Leaf 4 Leaf) 6 (Node Leaf 7 Leaf))
       False
       *Week02> treeMember 7 (Node (Node Leaf 4 Leaf) 6 (Node Leaf 7 Leaf))
       True

   'treeMember' has a similar structure to the 'isMember' function for
   searching lists, but now we can use the shape of the tree to guide
   our search. If the tree is well-balanced, then this will be much
   faster than searching a list. -}

{- To use binary search trees to implement key/value stores, we need to
   store pairs of keys and values instead of single data values. To do
   this, we could use the 'Pair' datatype we defined in Part 1.3, but
   it is easier to use a built in feature of Haskell for pair (and
   triple, and quadruple, ...) types. We can write any pair of values
   with a comma between them between parentheses to create a pair:

     *Week02> ("hello", 5)
     ("hello", 5)

   If we ask for the type, we get:

     *Week02> :t ("hello", 5)
     ("hello", 5) :: Num b => ([Char], b)

   which says that '("hello", 5)' is a pair of a list of 'Char's
   (a.k.a. a String), and any type 'b' that is a 'Num' type.

   Pattern matching on pairs uses the same '(,)' syntax: -}

getFirst :: (a,b) -> a
getFirst (x,y) = x

{- With pair types, we can represent key/value stores as binary search
   trees contains pairs of keys and values. We make a type synonym: -}

type KV k v = BST (k,v)

{- So the type 'KV String Int' contains values that are key/value stores
   mapping 'String' keys to 'Int' values.

   In the functions we write below, we will be careful to maintain the
   ordering constraints of binary search trees with respect to the
   keys but not the values. This makes sense: we will be looking up
   values by their keys, so it is the keys that we need to keep
   ordered.

   Here is an example of a simple key/value store mapping 'String's to
   'Int's: -}

store :: KV String Int
store = Node (Node Leaf ("a",1) Leaf) ("b",2) (Node Leaf ("c",3) Leaf)

{- This store maps "a" to 1, "b" to 2, and "c" to 3. Note that the keys
   are stored in order, and respect the 'left subtrees are smaller'
   and 'right subtrees are larger' constraint. -}

{- The first function we'll write is 'treeFind', which looks up a value
   by its key.

   The type of this function is similar to 'treeMember' above, but the
   return type is different. We return 'Maybe v': either the
   searched-for key has a value (so it returns 'Just v') or it doesn't
   (so it returns 'Nothing'). Otherwise, the function is similar to
   'treeMember' above: -}

treeFind :: Ord k => k -> KV k v -> Maybe v
treeFind k Leaf = Nothing
treeFind k (Node l (k',v') r)
  | k == k'   = Just v'
  | k < k'    = treeFind k l
  | otherwise = treeFind k r

{- Insertion into a key/value store is similar to the 'insertBST'
   function we wrote in Part 2.4, except that:

   - 'treeInsert' takes a key 'k' and a value 'v' to insert, instead
     of a single value.

   - 'treeInsert' specially handles the case when the key 'k' we are
     inserting is already present in the tree. In this case, we return
     a tree with the new value (v) and not the old one (v'). -}

treeInsert :: Ord k => k -> v -> KV k v -> KV k v
treeInsert k v Leaf = Node Leaf (k,v) Leaf
treeInsert k v (Node l (k',v') r)
  | k == k'   = Node l (k,v) r
  | k < k'    = Node (treeInsert k v l) (k',v') r
  | otherwise = Node l (k',v') (treeInsert k v r)


{-    Part 2.6 : BACKTRACKING SEARCH AND CASE EXPRESSIONS

   In the first part of this week, we looked at how functions can make
   decisions using 'if-then-else' and guards. Both of these are
   limited to testing conditions that return 'Bool'ean results that
   are either 'True' or 'False'. However, Haskell functions can return
   much more informative datatypes than just 'Bool', so we need a way
   to pattern match on values returned from functions, as well as the
   values passed into functions like we have done so far.

   To do this, we use a case expression. Here is an example: -}

treeMember2 :: Ord a => a -> BST a -> Bool
treeMember2 x Leaf = False
treeMember2 x (Node l y r) =
  case compare x y of
    EQ -> True
    LT -> treeMember2 x l
    GT -> treeMember2 x r

{- This is a rewrite of the 'treeMember' function, except that instead
   of using guards and a sequence of true/false conditions, we use a
   the comparison function 'compare' instead, with a 'case'
   expression.

   Let's take this step by step to understand what is going on. We can
   get the type of 'compare' from GHCi by using ':info':

        *Week02> :info compare
        class Eq a => Ord a where
          compare :: a -> a -> Ordering
          ...
                -- Defined in ‘ghc-prim-0.5.3:GHC.Classes’

   So 'compare' takes two 'Ord' things of the same type and returns an
   'Ordering'. Using ':info' again, we can see how 'Ordering' is
   defined:

        *Week02> :info Ordering
        data Ordering = LT | EQ | GT
                -- Defined in ‘ghc-prim-0.5.3:GHC.Types’

   So an 'Ordering' can be either 'LT' (less than), 'EQ' (equal) or
   'GT' (greater than). We might have a guess now at what 'compare'
   will do, but let's test it in GHCi:

        *Week02> compare 1 2
        LT
        *Week02> compare 2 2
        EQ
        *Week02> compare 2 1
        GT

   So we can use 'compare' to test two 'Ord'ered things, and it
   returns a value telling us how the two things are ordered. The
   'case' expression then allows us to return different things
   depending on the ordering:

        case compare x y of
          EQ -> True
          LT -> treeMember2 x l
          GT -> treeMember2 y r

   In general, a 'case' expression has the form:

        case <expression> of
          <pattern1> -> <code for case 1>
          ...
          <patternN> -> <code for case N>

   The patterns are of the same form as the ones that appear when
   writing the lines of a function definition. They can also have
   guards attached to them. Note that all the patterns have to line up
   in the same column, otherwise you will get a syntax error. -}

{- We finish this week with a larger example of a function that uses
   case expressions, and also stores information in an accumulator
   argument as it goes (similar to the 'reverse' example from last
   week).

   The problem we are going to look at is splitting up some amount of
   money with some coins, if this is a possible.

   For example, if we have the amount 54 and the coins
   [50,20,20,10,2,2,1,1], then there are several ways of splitting it
   up:

   - [2,2,50]
   - [1,1,2,50]
   - [2,2,10,20,20]
   - [1,1,2,10,20,20]

   We are going to write a function that searches for one way of
   splitting up an amount into some coins. It may not always be
   possible, so we will use a 'Maybe' to allow for the possibility
   that we will have to return 'Nothing'.

   First, let's make a type synonym for 'Coin's so that our types are
   easier to read: -}

type Coin = Int

{- Our first attempt at a function for making change takes the following approach:

   - The function will take three arguments:
     - the list of coins available for use
     - the list of coins that have been used so far
     - the amount that remains to be split

  - When the amount to be split is '0', then we have succeeded, so we
    return 'Just' the coins used so far.

  - When the amount to be split is not '0', and we have no coins
    available, we have failed, so we return 'Nothing'.

  - When we have coins available, we check to see whether or not the
    amount remaining to be split is greater than the first available
    coin. If it is, then we can use this coin: we add it to the 'used'
    list and subtract it from the current amount. We then call
    ourselves recursively to try the rest of the coins.

  The following definition implements this idea: -}

makeChange0 :: [Coin] -> [Coin] -> Int -> Maybe [Coin]
makeChange0 coins        used 0 = Just used
makeChange0 []           used _ = Nothing
makeChange0 (coin:coins) used amount
  | amount >= coin =
    makeChange0 coins (coin:used) (amount - coin)
  | otherwise =
    makeChange0 coins used amount

{- Let's try it on an example:

       *Week02> makeChange0 [50,20,20,10,2,2,1,1] [] 54
       Just [2,2,50]

   So it seems to work? What if we give it an odd selection of coins?

       *Week02> makeChange0 [50,20,20,11,2,1] [] 54
       Nothing

   But this should have worked! 20+20+11+2+1 = 54. What went wrong?

   We can see what went wrong by tracing out what the function does:

       makeChange0 [50,20,20,11,2,1] [] 54
     = makeChange0 [20,20,11,2,1] [50] 4
     = makeChange0 [20,11,2,1] [50] 4
     = makeChange0 [11,2,1] [50] 4
     = makeChange0 [2,1] [50] 4
     = makeChange0 [1] [50] 2
     = makeChange0 [] [50] 1
     = Nothing

  In the first step, 50 <= 54, so the function greedily tries to use
  '50', yielding the new amount '4' to split. The coins '20,20,11' are
  too big, and when we get down to '2' and '1' there aren't enough
  coins left.

  The problem is that we went for '50' immediately, without any
  possibility of revisiting our decision. A way to fix this is to
  allow backtracking. If a coin fits we give it a go and proceed
  having removed that coin. However, if that fails, we backtrack and
  try skipping over that coin.

  To program this in Haskell, we use a 'case' expression to try using
  a coin. If it fails (returns 'Nothing') we try again skipping over
  this coin: -}

makeChange :: [Coin] -> [Coin] -> Int -> Maybe [Coin]
makeChange coins        used 0 = Just used
makeChange []           used _ = Nothing
makeChange (coin:coins) used amount
  | amount >= coin =
    case makeChange coins (coin:used) (amount - coin) of
      Just coins -> Just coins
      Nothing    -> makeChange coins used amount
  | otherwise =
    makeChange coins used amount

{- This fixed function works on both examples:

       *Week02> makeChange [50,20,20,10,2,2,1] [] 54
       Just [2,2,50]
       *Week02> makeChange [50,20,20,11,2,1] [] 54
       Just [1,2,11,20,20]
-}

{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- In the questions below, replace 'undefined' with your answers. Use
   GHCi to test them.-}

{- 1. Write a function that counts the number of occurrences of an
      element in list: -}

popCount :: Eq a => a -> [a] -> Int
popCount = undefined

{-    (popCount is short for "population count"). Examples:

         popCount 2 [1,2,5,2,7,2,9] == 3
         popCount 9 [1,2,5,2,7,2,9] == 1
         popCount 0 [1,2,5,2,7,2,9] == 0
-}


{- 2. Write a version of 'insert' that only inserts into a sorted list
      if the element is not already there. Examples:

         insertNoDup 2 [1,3,4]   == [1,2,3,4]
         insertNoDup 2 [1,2,3,4] == [1,2,3,4]
-}

insertNoDup :: Ord a => a -> [a] -> [a]
insertNoDup = undefined


{- 3. Write a version of 'remove' that removes all copies of an element
      from a sorted list, not just the first one. Examples:

         removeAll 2 [1,2,2,3] == [1,3]
         removeAll 2 [1,3]     == [1,3]
-}

removeAll :: Ord a => a -> [a] -> [a]
removeAll = undefined


{- 4. Rewrite 'treeFind' and 'treeInsert' to use 'compare' and 'case'
      expressions. -}

treeFind2 :: Ord k => k -> KV k v -> Maybe v
treeFind2 = undefined

treeInsert2 :: Ord k => k -> v -> KV k v -> Maybe v
treeInsert2 = undefined


{- 5. MergeSort is another sorting algorithm that works in the following
      way:

      - If the list to be sorted is zero length, then it is already
        sorted.

      - If the list to be sorted has one element, then it is already
        sorted.

      - Otherwise, split the list into two, one with the even elements
        and one with the odd elements. Sort the two lists by calling
        'mergeSort' recursively. Then merge the two lists together
        maintaining the ordering.

      Write this function in three parts: -}

{-    'split' splits the input into two lists: one with the odd numbered
      elements and one with the even numbered elements. HINT: you can
      pattern match on multiple elements at the head of a list with
      'x1:x2:xs', and you can use the '(odds,evens) = ...' syntax in a
      'where' clause. -}

split :: [a] -> ([a], [a])
split = undefined

{-    'merge' merges two sorted lists into one sorted list. Examples:

          merge [1,3,5] [2,4,6]  = [1,2,3,4,5,6]
          merge [1,3,5] [7,9,11] = [1,3,5,7,9,11]
-}

merge :: Ord a => [a] -> [a] -> [a]
merge = undefined

{-    'mergeSort' uses 'split' and 'merge' to implement the merge sort
      algorithm described above. -}

mergeSort :: Ord a => [a] -> [a]
mergeSort = undefined


{- 6. Write another version of 'makeChange' that returns all the
      possible ways of making change as a list: -}

makeChangeAll :: [Coin] -> [Coin] -> Int -> [[Coin]]
makeChangeAll = undefined

{- HINT: you don't need a case expression, just a way of appending two
   lists of possibilities. -}
