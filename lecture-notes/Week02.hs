{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week02 where

{-   WEEK 2 : SOLVING PROBLEMS BY RECURSION -}

-- FIXME: copy from Lec03

{-   PART 1 : SEARCHING LISTS AND TREES -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show




{-   PART 2 : INSERTION SORT -}



{-   PART 3 : QUICKSORT -}



{-   PART 4 : TREESORT -}



{-   PART 5 : USING AN ACCUMULATOR -}

-- FIXME: explain why flatten is slow

-- FIXME: do reverse first

flattenAccum :: Tree a -> [a] -> [a]
flattenAccum Leaf accumulator = accumulator
flattenAccum (Node smaller x larger) accumulator =
  let accumulator0 = flattenAccum larger accumulator
      accumulator1 = x : accumulator0
      accumulator2 = flattenAccum smaller accumulator1
  in
  accumulator2

-- FIXME: or write it out in one line

-- FIXME: spell out how this works via rewriting

-- treesort2 = flattenAccum (listToTree xs) []

{-   PART 6 : BACKTRACKING -}

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
