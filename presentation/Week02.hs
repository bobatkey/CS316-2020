{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week02 where


{-   CS316 FUNCTIONAL PROGRAMMING 2020/21

         Week 2 : SOLVING PROBLEMS BY RECURSION

   This week, we continue our exploration of solving problems in
   Haskell by the use of recursion, and look at other ways to make
   decisions beyond pattern matching. -}





{-!      Part 2.1 : SEARCHING LISTS

  Searching a list of Integers:
-}

isMember0 :: Integer -> [Integer] -> Bool
isMember0 x []     = False
isMember0 x (y:ys) = if x == y then True else isMember0 x ys







{-! Making isMember generic -}

isMember :: Eq a => a -> [a] -> Bool
isMember x []     = False
isMember x (y:ys) = if x == y then True else isMember x ys











{-! Using guards -}

isMember1 :: Eq a => a -> [a] -> Bool
isMember1 x []     = False
isMember1 x (y:ys)
  | x == y    = True
  | otherwise = isMember1 x ys











{-! Using || -}

isMember2 :: Eq a => a -> [a] -> Bool
isMember2 x []     = False
isMember2 x (y:ys) = x == y || isMember2 x ys











{-! Assuming the list is sorted -}

isMember3 :: Ord a => a -> [a] -> Bool
isMember3 x [] = False
isMember3 x (y:ys)
  | x == y    = True
  | x < y     = False
  | otherwise = isMember3 x ys











{-! Summary

    - Functions can make descisions using if-then-else and guards, as
      well as pattern matching.

    - To be able to compare values, we need to use the 'Eq' or 'Ord'
      constraints as appropriate.
-}







{-!      Part 2.2 : INSERTING AND REMOVING ELEMENTS

  Inserting elements into a sorted list. -}

-- 2 [1,2,3]  =>  [1,2,2,3]

insert :: Ord a => a -> [a] -> [a]
insert x [] = [x]
insert x (y:ys)
  | x < y     = x : y : ys
  | otherwise = y : insert x ys

--   insert 2 [1,2,3]
-- = 1 : insert 2 [2,3]
-- = 1 : 2 : insert 2 [3]
-- = 1 : 2 : 2 : 3 : []
-- = [1,2,2,3]









{-! Removing elements -}

remove :: Ord a => a -> [a] -> [a]
remove x [] = []
remove x (y:ys)
  | x == y    = ys
  | x < y     = y:ys
  | otherwise = y:remove x ys











{-! Summary

  - Inserting into a sorted list involves a search to find the right
    place that rebuilds the list.

  - Removal from a sorted list is similar.

-}







{-!      Part 2.3 : INSERTION SORT AND QUICKSORT -}

{- insert :: Ord a => a -> [a] -> [a] -}

isort :: Ord a => [a] -> [a]
isort []     = []
isort (x:xs) = insert x (isort xs)









{-! isort is slow -}

--   isort [3,2,1]
-- = insert 3 (isort [2,1])
-- = insert 3 (insert 2 (isort [1]))
-- = insert 3 (insert 2 (insert 1 []))
-- = insert 3 (insert 2 [1])
-- = insert 3 [1,2]
-- = [1,2,3]













{-! Quicksort -}

qsort :: Ord a => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
  where smaller = [ y | y <- xs, y < x ]
        larger  = [ y | y <- xs, y >= x ]











{-! Quicksort structure -}

{-
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
-}

{-! Quicksort structure -}

{-
                       [5]
                  [3]      []
              [1]    []
            []  [2]
               [] []
-}





{-! Summary

    - insertion sort is easily written by doing repeated insertions into a sorted list.

    - quicksort (sort of) can be written as a divide and conquer algorithm.

    - But quicksort has a complex recursion structure.
-}







{-!      Part 2.4 : TREESORT -}

data BST a
  = Leaf
  | Node (BST a) a (BST a)
  deriving Show

insertBST :: Ord a => a -> BST a -> BST a
insertBST x Leaf = Node Leaf x Leaf
insertBST x (Node l y r)
  | x < y     = Node (insertBST x l) y r
  | otherwise = Node l y (insertBST x r)






{-! Inserting all elements -}

listToTree :: Ord a => [a] -> BST a
listToTree []     = Leaf
listToTree (x:xs) = insertBST x (listToTree xs)











{-! Flattening a Tree -}

flatten :: BST a -> [a]
flatten Leaf         = []
flatten (Node l x r) = flatten l ++ [x] ++ flatten r











{-! Sorting -}

treesort :: Ord a => [a] -> [a]
treesort xs = flatten (listToTree xs)











{-! Summary

   - Tree sort exposes the underlying structure of Quicksort

   - Choosing the right data structure can make a problem simpler and
     reveal extra ideas.
-}








{-!      Part 2.5 : SEARCHING AND UPDATING TREES -}

{- data BST a
     = Leaf
     | Node (BST a) a (BST a)
     deriving Show
-}


treeMember :: Ord a => a -> BST a -> Bool
treeMember x Leaf = False
treeMember x (Node l y r)
  | x == y    = True
  | x < y     = treeMember x l
  | otherwise = treeMember x r




{-! Key/Value stores

  Examples...
-}

type KV k v = BST (k,v)

store :: KV String Int
store = Node (Node Leaf ("a",1) Leaf) ("b",2) (Node Leaf ("c",3) Leaf)







{-! treeFind -}

treeFind :: Ord k => k -> KV k v -> Maybe v
treeFind k Leaf = Nothing
treeFind k (Node l (k',v') r)
  | k == k'   = Just v'
  | k < k'    = treeFind k l
  | otherwise = treeFind k r











{-! treeInsert -}

treeInsert :: Ord k => k -> v -> KV k v -> KV k v
treeInsert k v Leaf = Node Leaf (k,v) Leaf
treeInsert k v (Node l (k',v') r)
  | k == k'   = Node l (k,v) r
  | k < k'    = Node (treeInsert k v l) (k',v') r
  | otherwise = Node l (k',v') (treeInsert k v r)












{-! Summary

    - Binary Search Trees are useful for more than just sorting

    - Haskell has built in pair, triple, etc. types

    - Binary search trees full of pairs can be used to represent key/value stores
-}







{-!      Part 2.6 : BACKTRACKING SEARCH AND CASE EXPRESSIONS -}



treeMember2 :: Ord a => a -> BST a -> Bool
treeMember2 x Leaf = False
treeMember2 x (Node l y r) =
  case compare x y of
    EQ -> True
    LT -> treeMember2 x l
    GT -> treeMember2 x r

-- case <expression> of
--   <pattern1> -> <code1>
--   ...
--   <patternN> -> <codeN>










{-! Making change

  If we have the coins

     [50,20,20,10,2,2,1,1]

  and we want to make 54p, then there are four ways:

   - [2,2,50]
   - [1,1,2,50]
   - [2,2,10,20,20]
   - [1,1,2,10,20,20]
-}












{-! Making change function -}

type Coin = Int

testCoins :: [Coin]
testCoins = [50,20,20,10,2,2,1,1]

makeChange :: [Coin] -> [Coin] -> Int -> Maybe [Coin]
makeChange coins        used 0 = Just used
makeChange []           used n = Nothing
makeChange (coin:coins) used n
  | coin <= n = case makeChange coins (coin:used) (n - coin) of
                  Nothing -> makeChange coins used n
                  Just change -> Just change
  | otherwise = makeChange coins used n

-- makeChange [50,20,20,11,2,1] [] 54
-- makeChange [20,20,11,2,1] [50] 4
-- ...
-- makeChange [2,1] [50] 4
-- makeChange [1] [2,50] 2
-- makeChange [] [1,2,50] 1
-- Nothing
