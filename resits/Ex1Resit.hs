module Ex1Resit where

{------------------------------------------------------------------------------}
{- CS316 (2020/21 RESIT) EXERCISE 1 : FIRST-ORDER PROGRAMMING                 -}
{------------------------------------------------------------------------------}

{- The resit questions consist of:

     1. Exercise 1 (30 marks) (this one)
     2. Exercise 2 (30 marks)
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
{- PART 1 : LISTS AND TREES                                           -}
{----------------------------------------------------------------------}

{- 1.1 Concatenation. The infix operator ++ concatenates two lists. Use
   it to write a function in pattern matching style which concatenates
   a list of lists. We have given you an unfinished definition which
   you should refine into suitable cases and complete. -}

concLists :: [[x]] -> [x]
concLists xss = undefined    -- complete this definition

{- It may help to think concretely:
   (a) What should
     concLists [[1], [2,3], [4,5,6]]
   be?
   (b) What should
     concLists [[2,3], [4,5,6]]
   be?
   (c) How do you combine [1] with the answer to (b) to make the answer
   to (a)?
-}

{- 2 MARKS -}


{- 1.2 Mirroring trees. -}

data Tree x
  = Leaf
  | Node  (Tree x) x (Tree x)
  deriving Show  -- so you can see what you're doing!

{- Write a function to compute the mirror image of a tree, with left
   and right subtrees swapped at every level. For example

   mirror (Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))
                4
                (Node (Node Leaf 5 Leaf) 6 (Node Leaf 7 Leaf)))

   should be
           Node (Node (Node Leaf 7 Leaf) 6 (Node Leaf 5 Leaf))
                4
                (Node (Node Leaf 3 Leaf) 2 (Node Leaf 1 Leaf)))
-}

mirror :: Tree x -> Tree x
mirror t = undefined       -- you write this

{- Think concretely. Use the above example to figure out how mirror
   should work. -}

{- 3 marks -}

{- QUESTION: For a (finite) tree t, what should
   mirror (mirror t)
   be?
   ANSWER: write your answer here
-}

{- 1 MARK -}

{- 1.3 Right spines -}

{- The "right spine" of a binary tree is the list of node labels starting
   at the root, and descending into each right subtree until the rightmost
   leaf is reached. Write a function to compute the right spine of a tree. -}

rightSpine :: Tree x -> [x]
rightSpine t = undefined     -- you write this

{- For example, you should have
   rightSpine (Node (Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf))
                    4
                    (Node (Node Leaf 5 Leaf) 6 (Node Leaf 7 Leaf)))
   = [4,6,7]
-}

{- 2 MARKS -}


{- Assuming you can test values only for equality, we can write a function
   to test if a given value is in a given tree.  -}

eqFindInTree :: Eq x => x -> Tree x -> Bool
eqFindInTree x Leaf         = False
eqFindInTree x (Node l y r) = x == y || eqFindInTree x l || eqFindInTree x r

{- 1.4 Finding in a tree.

   Your mission is to test if a given value is somewhere in a tree
   efficiently, under the assumption that the tree is a /binary search
   tree/:

       A tree is a binary search tree if:
       1. it is 'Leaf'; or
       2. it is 'Node l x r' and all of the following are true:
          (a) every value in l is <= x
          (b) every value in r is >= x
          (c) l is a binary search tree
          (d) r is a binary search tree

   Assuming you can test values for order and equality (with <, >, ==),
   as given by the "Ord x =>" constraint in the type, and that the
   input trees are binary search trees, write an *efficient* function
   which tests if an element is in a tree.
   It may help you to write out some example binary search trees and
   to think about how to search them before writing this function. -}

ordFindInTree :: Ord x => x -> Tree x -> Bool
ordFindInTree x t = undefined -- write this function

{- 4 MARKS -}


{- 1.5 Functions act strangely if their assumptions are not
   satisfied. The 'ordFindInTree' function expects its input to be a
   binary search tree.  Give an example of a tree and a value to
   search for that gives different answers with 'eqFindInTree' and
   'ordFindInTree'. -}

differentiatingTree :: Tree a   -- replace 'a' with a type of your choice
differentiatingTree = undefined

differentiatingValue :: a  -- and here
differentiatingValue = undefined

{- 4 MARKS -}

{----------------------------------------------------------------------}
{- MODELLING COMMUNICATING PROCESSES                                  -}
{----------------------------------------------------------------------}

{- This exercise is about modelling processes which input and output
   bits. Processes are things. They're a kind of tree, representing a
   decision process, given by the following datatype. -}

{- We'll do the setup, then it'll be your turn. -}

data Process
  = End    -- marks the end of the process, so no more input or output
  | Output Bool Process
           -- (Output b p) outputs bit b, then continues as p
  | Input Process Process
           -- (Input tp fp) inputs a bit, continuing as tp if it's
           -- True, fp if False
  deriving Show

{- Don't expect the data in this type to *do* anything! Rather, they
   *represent* processes. We'll see how to interpret them shortly. -}

{- Let's have an example process: this process should output False if its
   input is True and True if its input is False. -}

notGate :: Process
notGate = Input (Output False End) (Output True End)

{- See? If the input is True, we take the left path and find
   (Output False End), otherwise we go right and find (Output True End).
   Either way, we make one output and then stop. -}

{- How can we make processes go? We need to interpret them. Here's how.
   The "process" function takes a Process to interpret, and a list of input
   bits in [Bool], then produces the list of output bits. -}

process :: Process -> [Bool] -> [Bool]
process End            bs        = []
  -- when we're at the end, there is no more output
process (Output b p)   bs        = b : process p bs
  -- the output from (Output b p) had better begin with b, and the rest
  -- is whatever comes out from running p on the input
process (Input tp fp)  (b : bs)  = process (if b then tp else fp) bs
  -- when the process wants input, the result depends on the first bit
  -- in the input list: if that's True, then we continue with the tp
  -- branch; if it's false, we continue with the fp branch. In both
  -- cases, we feed the rest of the input bs to the continuing process
process (Input tp fp)  []        = []
  -- in the unfortunate case where the process wants input but the input
  -- list is empty, we're a bit stuck; let's stop and return no output

{- Let's try it out. Here are some test examples. Try loading this file
   in ghci, then evaluating testNotT and testNotF at the prompt. Do
   you get what you expect? -}

testNotT :: [Bool]
testNotT = process notGate [True]

testNotF :: [Bool]
testNotF = process notGate [False]


{- 1.6 The Wire.

   Make a process, rather like the notGate, which reads one bit of
   input and sends that one bit unaltered to the output. -}

wire :: Process
wire = undefined     -- you define it

{- 1 MARK -}


{- use ghci to check that
     process wire [True]   =  [True]
     process wire [False]  =  [False]
-}

{- Let's make some gates with two inputs. Here's 'or'. -}

orGate :: Process
orGate = Input (Input (Output True End) (Output True End))
               (Input (Output True End) (Output False End))

{- use ghci to check that
     process orGate [True,True]    = [True]
     process orGate [True,False]   = [True]
     process orGate [False,True]   = [True]
     process orGate [False,False]  = [False]
-}


{- 1.7 Xor, And.

   Make processes in the style of orGate which act like an xor-gate
   and an and-gate, respectively. -}

xorGate :: Process
xorGate = undefined  -- you define it

andGate :: Process
andGate = undefined  -- you define it

{- 3 MARKS -}

{- you can use ghci to check these also work as they should -}


{- 1.8 Ignoring input.

   You can see there's quite a bit of repetition in orGate and
   andGate. For example, if the first input to orGate is True, then
   the output must be True, irrespective of the second input. But we
   do have to consume the input, even if it makes no difference. Write
   a function... -}

always :: Process -> Process
always p = undefined     -- you define it

{- ... which takes a process p and produces the process which reads
   one input, then ignores it and acts like p in *both* cases. -}

{- 1 MARK -}

{- You should find that you can simplify your gates. If you've got
   "wire" and "always" right, you should find this new definition
   of orGate works just the same as the old one. -}

orGate' :: Process
orGate' = Input (always (Output True End)) wire

{- See? If the first input is True, then the output is always True,
   regardless of the second. If the first input is False, then the
   process behaves like a wire, transmitting the second input as is. -}


{- 1.9 Tidy up your xorGate and andGate in the same way. Make use of
   wire, notGate, always, etc. Small is beautiful. -}

xorGate' :: Process
xorGate' = undefined     -- you define it

andGate' :: Process
andGate' = undefined     -- you define it

{- 2 MARKS -}

{- 1.10 May output False?

   A process may output False if there is any sequence of inputs that
   will make it output 'False'. Because we have descriptions of
   processes, we can "look ahead" to see whether or not they will
   output False:

    - 'End' never outputs False.
    - 'Output True p' may outputs False if 'p' may output False.
    - 'Output False p' may output False
    - 'Input p1 p2' may output False if at least one of 'p1' or 'p2' may output False.

   Write a function 'mayOutputFalse' that returns 'True' if the
   process it is given may output False, and 'False' otherwise. -}

mayOutputFalse :: Process -> Bool
mayOutputFalse = undefined

{- 4 MARKS -}

{- 1.11 May output 'x'?

   Generalise your answer to the previous question to write a function
   that determines whether or not a process may output 'x', where 'x'
   is either 'True' or 'False' and is given as another argument. -}

mayOutput :: Bool -> Process -> Bool
mayOutput = undefined

{- 2 MARKS -}

{- 1.12 Must output.

   A process *must* output a boolean 'x' if it will output 'x' for
   *any* input. Write a function (similar to 'mayOutput') that returns
   'True' if the process must output the given 'x' and 'False'
   otherwise. -}

mustOutput :: Bool -> Process -> Bool
mustOutput = undefined

{- 4 MARKS -}

{----------------------------------------------------------------------}
{- END OF EXERCISE                                                    -}
{----------------------------------------------------------------------}
