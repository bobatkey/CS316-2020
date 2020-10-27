{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week06 where

import Prelude hiding (return, Either (..))

{-    WEEK 6 : SIMULATING SIDE EFFECTS

   Haskell is famous for not having "side effects", or being "purely"
   functional. What does this mean? Is it a good thing? Is it a bad
   thing?

   Haskell is based around the idea that programs are functions in a
   mathematical sense. They take some inputs and produce an output,
   and (crucially) nothing else. This means that, if we have a
   function with the following type:

        f : Int -> Int

   then we know, from the type, that 'f' is a function that, when we
   give it an 'Int', we will get back an 'Int' (assuming it
   terminates), and nothing else will happen.

   This is in contrast with a language like Java. The analogous method
   signature in Java for 'f' is:

        int f(int x)

   In Java, we know that 'f' is a method that takes 'int's and returns
   'int's, but it may also do any amount of "side effects", extra
   behaviour beyond what is explicitly listed:

    - 'f' may print things to the screen.
    - 'f' may throw 'Error's.
    - 'f' may open network connections and purchase goods from Amazon,
      hack into nuclear weapons systems, or post cat pictures to
      Facebook.

   We have no way of knowing from the method signature whether the 'f'
   method will do any, all, or none of these things.

   So Haskell and Java have different defaults. By default a Haskell
   function is restricted to doing nothing "on the side". In Java, by
   default a method is allowed to do anything on the side. (Strictly
   speaking this isn't totally true: a Java method signature can
   restrict the 'Exception's that may be thrown by using the 'throws'
   keyword, however a Java method can always throw an object whose
   class is a subclass of java.lang.RuntimeException, which doesn't
   need to be listed in the 'throws' list.)

   Both defaults have their tradeoffs. Java's default is liberating
   because you can do whatever you want, but limiting because you have
   no machine checkable way of making sure that a particular method
   *doesn't* do some side effect, aside from writing it in the
   documentation. Haskell's default is liberating because we can
   always be sure to know that nothing will happen on the side, which
   can help with reasoning about and refactoring programs, but
   limiting because we currently have no way of doing side effects!

   So how do we use side effects in Haskell programs? They surely are
   useful, because they are used all the time in other languages.

   We will build our way to the answer by first seeing how we can
   *simulate* side effects in Haskell, and looking at the common
   patterns that arise. First we look at how to simulate the side
   effect of "possibly throwing an exception". -}


{-    Part 6.1 : SIMULATING EXCEPTIONS

   We've seen the 'Maybe' type several times so far in this course, as
   a way of making it explicit when a function can possibly fail to
   return a value.

   To recap, if a function promises to return an 'Int', then it has to
   return an 'Int'. If it promises to return a 'Maybe Int', then it
   can either return 'Nothing', or 'Just x', where 'x' is an 'Int'.

   Let's use 'Maybe' to simulate throwing exceptions. We'll think of
   'Nothing' as "throwing an exception", and 'Just x' as "returning
   normally with 'x'".

   To make this idea clearer, let's make two short definitions to give
   'Nothing' and 'Just' slightly different names: -}

failure :: Maybe a
failure = Nothing

returnOk :: a -> Maybe a
returnOk x = Just x

{- So now, instead of writing 'Nothing' we can write 'failure' and
   instead of writing 'Just x', we can write 'return'. (Note that,
   unfortunately, we can only do this in expressions, not in
   patterns. Oh well.)

   Now let's write a function using this. The function 'subtractOne'
   takes an 'Int' and subtracts 1 from it; unless the input is 0 or
   less, in which case it throws an exception: -}

subtractOne :: Int -> Maybe Int
subtractOne x = if x > 0 then
                  returnOk (x-1)
                else
                  failure

{- We can think of this as similar to the Java static method:

      static int subtractOne(int x) {
           if (x > 0)
                   return x-1;
           else
                   throw new IllegalArgumentException​();
      }


   (We'll discuss how to have different sorts of 'failure' below,
   corresponding to the different possible exception classes in Java.)

   A more useful function is one that searches for a 'k'ey in a list
   of 'k'ey/'v'alue pairs. Such a search may fail if the key is not
   there, and succeeds (returns normally) if it does find the key. We
   write this like so: -}

search :: Eq k => k -> [(k,v)] -> Maybe v
search k [] = failure
search k ((k',v'):kvs) =
  if k == k' then
    returnOk v'
  else
    search k kvs

{- Now let's try to use this function to build a larger one. We're going
   to write a function that takes a list of key/value pairs, and a
   tree full of keys, and returns a tree of the same shape, but with
   all the keys replaced with their corresponding values from the
   list.

   Here's the 'Tree' type again: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- Now we write the function described above. Since searching for a key
   in a list of key/value pairs may fail, looking up all the keys in a
   'Tree' may fail, so the function can only promise to return a
   'Maybe (Tree v)': -}
lookupAll :: Eq k => [(k,v)] -> Tree k -> Maybe (Tree v)

{- The actual traversal of the Tree pattern matches on whether the input
   Tree is a 'Leaf' or a 'Node'. In the 'Leaf' case, we return the
   'Leaf', using our 'return' helper: -}
lookupAll kvs Leaf = returnOk Leaf

{- In the 'Node l k r' case, we have to do a lot more work. In essence,
   we want to first look up all the keys in the 'l'eft subtree, look
   up the 'k'ey at this node, look up all the keys in the 'r'ight
   subtree and then put the three results back together into a
   'Node'. However, writing this out gets quite complex because each
   of these operations may fail: there may be missing keys in the left
   subtree, the key at this node may be missing, or there may be
   missing keys in the right subtree. So, after each operation, we
   have to match on whether it succeeded or not. If an operation did
   not succeed (i.e., returned 'Nothing'), then the whole thing
   returns 'Nothing'. If an operation does succeed, then we continue
   to the next operation. We can write this behaviour -- simulating
   what exceptions do -- out in full by using a 'case' for each
   operation: -}
lookupAll kvs (Node l k r) =
  case lookupAll kvs l of
    Nothing -> failure
    Just l' ->
      case search k kvs of
        Nothing -> failure
        Just v  ->
          case lookupAll kvs r of
            Nothing -> failure
            Just r' ->
              returnOk (Node l' v r')

{- The structure is the same for each operation ('lookupAll kvs l',
   'search k kvs', 'lookupAll kvs r'). If we call the operation 'op',
   then each step has the same structure:

      case op of
        Nothing -> failure         -- 'op' failed, return 'failure'
        Just x  -> .. carry on ... -- 'op' succeeded, call the result 'x' and carry on

   With three possibly failing operations like this, the "staircase"
   or "cascade" of 'cases' looks nearly bearable. But what if we had
   four, five, six, or twenty operations to be carried out? Following
   this structure would lead to massively indented code marching off
   the side of the screen. Worse, it is highly repetitive code,
   raising the possibility of tiny mistakes creeping in due to the
   difficulty of seeing the differences between many copies of the
   same bit of code.

   So is there a way of fixing this repetition? The key repeated piece
   of code is the 'case op of ...' snippet above. Can we abstract this
   out into a helper function? Following the idea of abstraction by
   taking concrete pieces of functions and giving them names that we
   saw in Week 3, we can do this. The first it to abstract out is the
   operation 'op', which must have type 'Maybe a', for some 'a'. The
   second bit is the section marked '... carry on ...'  above. It is
   less obvious how to abstract this, but the key is to notice that it
   is a piece of code that (a) must return something of type 'Maybe
   b', in order to match the 'failure' on the other branch, and that
   (b) it has access to an additional 'x' parameter. These
   observations lead to the idea that we can rewrite the snippet above
   to:

       case op of
         Nothing -> failure
         Just x  -> k x

   where 'op' has type 'Maybe a', and 'k' has type 'a -> Maybe b'. We
   can now turn this into a higher order function, capturing the
   pattern of "try 'op', and if that works, run 'k' with the
   result". We'll call the function 'ifOK' since it checks its first
   argument to see if it is OK ("returns normally") and if it is, it
   runs its second argument. -}

ifOK :: Maybe a -> (a -> Maybe b) -> Maybe b
ifOK op k = case op of
              Nothing -> Nothing
              Just a  -> k a

{- (Note: the second argument is called 'k' because it is short for
   "continuation" ('k' instead of 'c' is "traditional"). A
   continuation (of a program) is "what happens next".)

   Let's now see how we can use 'ifOK' to simplify our 'lookupAll'
   function. Here's the function again, but with the old 'case's
   commented out and replaced by the corresponding use of 'ifOK'. The
   first two lines are the same, all the changes are in the second
   case: -}

lookupAll_v2 :: Eq k => [(k,v)] -> Tree k -> Maybe (Tree v)
lookupAll_v2 kvs Leaf = returnOk Leaf
lookupAll_v2 kvs (Node l k r) =
  ifOK (lookupAll_v2 kvs l) (\l' ->
--  case lookupAll kvs l of
--    Nothing -> failure
--    Just l' ->
  ifOK (search k kvs) (\v ->
--      case search k kvs of
--        Nothing -> failure
--        Just v  ->
  ifOK (lookupAll_v2 kvs r) (\r' ->
--          case lookupAll kvs r of
--            Nothing -> failure
--            Just r' ->
              returnOk (Node l' v r'))))

{- Instead of doing pattern matches and a cascade of 'cases', we are
   capturing the behaviour of exceptions in the function 'ifOK',
   passing in the possibly failing operation each time as the first
   argument, and the continuation (what to do next) as the second
   argument. Removing the commented out code shows what a space
   benefit this has been: -}

lookupAll_v3 :: Eq k => [(k,v)] -> Tree k -> Maybe (Tree v)
lookupAll_v3 kvs Leaf = returnOk Leaf
lookupAll_v3 kvs (Node l k r) =
  ifOK (lookupAll_v3 kvs l) (\l' ->
  ifOK (search k kvs)       (\v ->
  ifOK (lookupAll_v3 kvs r) (\r' ->
  returnOk (Node l' v r'))))

{- A further 'prettification' is to use 'ifOK' as an infix operator,
   putting it between the operation and the continuation
   arguments. This gives: -}

lookupAll_v4 :: Eq k => [(k,v)] -> Tree k -> Maybe (Tree v)
lookupAll_v4 kvs Leaf = returnOk Leaf
lookupAll_v4 kvs (Node l k r) =
  lookupAll_v4 kvs l  `ifOK` \l' ->
  search k kvs        `ifOK` \v ->
  lookupAll_v4 kvs r  `ifOK` \r' ->
  returnOk (Node l' v r')

{- We can now read this as a sequence of steps:

     1. Lookup the keys in 'l', if that is OK, call the result l'
     2. Search for 'k' in 'kvs', if that is OK, call the result 'v'
     3. Lookup the keys in 'r', if that is OK, call the result r'
     4. Finally, return the Node made with l', v, and r'.

   With a bit of artistic licence, we could think of 'ifOK' as a kind
   of "semicolon" fitting in between the operations performed by our
   program, telling Haskell how to run one operation before another.

   In fact, the type signature of 'ifOK' demonstrates a very useful
   pattern. Let's look at it again:

      ifOK :: Maybe a -> (a -> Maybe b) -> Maybe b

   The first argument is some operation to perform, which may
   fail. The second argument is some operation to perform, if the
   first argument succeeds.

   This pattern is very general. If we replace 'Maybe' with some 'M',
   we get:

        M a -> (a -> M b) -> M b

   And we think of 'M a' as "perform some side effects, which may
   eventually result in a value of type 'a'", then this type expresses
   what we need to define a sequencing operation.

   In the next lecture, we'll see another example of this idea, but
   with stateful computations (i.e., using mutable variables) instead
   of exceptions. Next week, we'll give this pattern a name: Monad. -}



{-    Part 6.2 : CATCHING EXCEPTIONS

   The 'ifOK' function captures one aspect of programming with
   exceptions: that if we execute two possibly failing programs in
   sequence, and the first one fails, then everything fails. But, of
   course, it is also possible to catch exceptions and handle
   them. Can we simulate this in Haskell?

   Because we are simulating exceptions with values of type 'Maybe a',
   we can simulate the effect of catching an exception by pattern
   matching on the result of a failable operation. If the operation
   fails, then we do the "handler" code, otherwise we return the
   result.

   Let's see this in action with a simple wrapper for 'search' that
   uses a default value if the searched for key is not in the
   key/value list: -}

searchWithDefault :: Eq k => k -> Int -> [(k,Int)] -> Maybe Int
searchWithDefault k v kvs =
  case search k kvs of
    Nothing -> returnOk v
    Just a  -> Just a

{- So we run 'search k kvs' to search for 'k' in 'kvs'. If this returns
   'Nothing' (i.e., "throws an exception"), then we "handle" that
   eventuality by doing 'return v'. Otherwise, we pass through the
   returned value 'a'.

   Using a 'case' like this works, but it suffers from the same
   problem as the use of 'case' in 'lookupAll' above. If we have lots
   of 'case's like this, it gets hard to work out what the intended
   behaviour is.

   For 'lookupAll' above, we fixed this problem by defining 'ifOK' to
   capture the pattern of "try this, and if it works, do that". Can we
   capture the pattern of "try this, and if it fails, do that"?

   Let's do this by writing a function 'catch', which looks very
   similar to 'ifOK':  -}

catch :: Maybe a -> Maybe a -> Maybe a
catch op h = case op of
               Nothing -> h
               Just a  -> Just a

{- We can now rewrite 'searchWithDefault' using 'catch' to make the
   exception handling behaviour more explicit: -}

searchWithDefault_v2 :: Eq k => k -> Int -> [(k,Int)] -> Maybe Int
searchWithDefault_v2 k v kvs =
  catch (search k kvs)
        (returnOk v)

{- As above, writing 'catch' in between its arguments can make it easier
   read: -}

searchWithDefault_v3 :: Eq k => k -> Int -> [(k,Int)] -> Maybe Int
searchWithDefault_v3 k v kvs =
  search k kvs `catch` returnOk v

{- The 'catch' function demonstrates another common pattern that we'll
   see more of: being able to try one operation, and if that fails,
   trying something else. This pattern is less common than Monads (it
   won't apply in the case of mutable state, for example), but we'll
   see it come back when we cover Parsing.

      Exceptions with more information

   The 'Maybe' type above is useful for demonstrating the idea of
   simulating exceptions, but in practice it isn't very useful
   because, in the case of failure, it just says 'Nothing', and gives
   no clue what the exception was.

   A better type to use is the built-in type 'Either'. I have
   redefined the type here so you can see it: -}

data Either a b
  = Left a
  | Right b
  deriving Show

{- 'Either a b' is either 'Left a' or 'Right b'. We can think of 'Left
   a' as being like 'Just a', and 'Right b' as like 'Nothing', but
   with a value attached.

   EXERCISE:

     Rewrite 'ifOK' and 'catch' above to use 'Either a b' instead of
     'Maybe a'. Optionally, you can change the type signature of
     'catch' to pass the error message into the handler.

     Once you've done this, rewrite 'search' to return 'Either v
     String', with a useful error message when the key is not found. -}


{-    Part 6.3 : SIMULATING MUTABLE STATE

   In the last part, we saw how to simulate exceptions in Haskell by
   using the 'Maybe' type. However, writing out programs that use
   exceptions in this way was tedious due to the constant use of
   'case' to describe how exceptions behave.

   We found that it was possible to simplify programs that use
   simulated exceptions by defining a helper function, called 'ifOK',
   with the type:

      ifOK :: Maybe a -> (a -> Maybe b) -> Maybe b

   The idea behind 'ifOK' is that the first argument represents the
   result of some operation that may succeed or fail. If it succeeds,
   then the result of the operation is used to run the second
   operation (the "continuation", a function waiting on result of the
   first). The final result is either 'Nothing' if either operation
   fails, or 'Just x' if both operations succeed.

   We saw that 'ifOK' was useful for tidying up the definition of
   programs that are best expressed in terms of exceptions, instead of
   purely functionally. What about other forms of side effect, such as
   mutable state, or printing. Can they be tidied up in the same way?
   Let's find out... -}

{- First, we'll look at simulating mutable state in Haskell. Mutable
   state is the kind of variables that you are used to in a language
   like Java. Variables in Java can be mutated by setting them to new
   values. For example,

      int x = 5;
       .. some code where x is 5 ..
      x = x + 5;
       .. some code where x is 10 ..
      while (<blah>) {
         x += 1;
         .. x gets 1 bigger every time the loop executes ..
      }
      .. x could be some number bigger than 10 ..

   In Haskell, variables are immutable. Once they are given a value,
   they keep this value forever.

   We can simulate the behaviour of mutable variables using immutable
   variables by making a new variable each time we would have done a
   mutation. If we are careful to only use the most recently created
   variable, then we can simulate mutable state with immutable state.

   Here, in a pseudo code mash up of Java and Haskell, is how this
   would look. Every time we mutated a variable before, we define a
   new variable.

      int x0 = 5;
       .. some code where x0 is 5 ..
      int x1 = x0 + 5;
       .. some code where x1 is 10 ..
      loop x =
        int x2 = x + 1
        ...
        if not stop then loop x2 else x2
      int x3 = loop x1
      .. x3 could be some number bigger than 10 ..

   Sometimes, the new variables are called "versions" of the mutable
   variable 'x'.  We'll use this idea to simulate state in
   Haskell. (This technique is also used in modern compilers for
   compiling code that uses mutable state: Static Single Assignment
   form (SSA) turns each mutable variable into multiple copies, so
   that old versions can be referred to by a name.)

   Let's see this idea in action on a simple example. We'll use the
   datatype of 'Tree's again.

   The example problem we're going to try to solve this time is how to
   go through a tree, numbering each node from left to right. For
   example, we want to take the tree:

      Node (Node Leaf "A"     Leaf) "B"     (Node Leaf "C"     Leaf)

   and number it like so:

      Node (Node Leaf ("A",0) Leaf) ("B",1) (Node Leaf ("C",2) Leaf)

   If we think about how we might do this in a language with mutable
   state, we might have a variable that keeps track of which number we
   have counted up to which we increment every time we visit a node.

   In Haskell, we don't have mutable state by default, so we'll have
   to simulate it. The simulation works by passing the value of the
   state around as an additional parameter that is passed into
   functions and out again.

   We will solve the numbering problem by writing a function that
   passes over the tree from left to right, taking the current value
   of the counter as an argument, and returning the updated value of
   the counter as an additional result.

   Here is the function: -}

numberTree :: Tree a -> Int -> (Int, Tree (a, Int))
{- The type indicates that (a) it takes a 'Tree a' as input, and also an
   'Int' representing the current value of the counter; and (b) that
   it returns a pair of the new value of the counter, and the numbered
   tree. -}

numberTree Leaf counter =
  (counter, Leaf)
{- For the 'Leaf' case, we don't update the counter, because we are only
   counting Nodes. In our simulation, we return the counter
   unmodified, along with the Tree. -}

numberTree (Node l x r) counter =
  let (counter0, l') = numberTree l counter
      number         = counter0
      counter1       = counter0+1
      (counter2, r') = numberTree r counter1
  in (counter2, Node l' (x, number) r')
{- The 'Node' case is more complex. In overall structure it is similar
   to the cascade of 'case's in the use of 'Maybe' to simulate
   exceptions from the previous lecture. First we run 'numberTree l'
   to number the left subtree, passing in the value of the
   counter. This returns the new value of the counter, which we call
   'counter0', and the numbered left subtree. Then we take the current
   value of the counter ('counter0') and call it 'number'. Then we add
   one to the value 'counter0' to get 'counter1', simulating the step
   of incrementing the counter. Then we run 'numberTree r' to number
   the right subtree, passing in the current value of the counter
   ('counter1'), yielding another new value of the counter,
   'counter2'. Finally, we return the final value of the counter
   'counter2', and the numbered tree. -}


{- Phew! This is messy, and the opportunity to screw up here is
   immense. We have had to be careful to number variables
   consistently, and to always use the most recent variable name
   holding the "current" value of a variable.

   Can we tidy up the mess? Let's try by first isolating a type of
   "simulations of state mutating operations", just as we used 'Maybe'
   as a type of "simulations of fallible operations". Looking at the
   type of 'numberTree', the following looks like it might work: -}

type State a = Int -> (Int, a)

{- With this definition we are saying "A state mutating operation that
   returns 'a's is a function that takes 'Int's (the initial state) and
   returns pairs of 'Int's (the final state) and 'a's (the result value).

   Just as wrote a function called 'return :: a -> Maybe a' to
   simulate fallible operations that return a value without failing,
   we'll also need a function that simulates mutating operations that
   return a value without mutating anything. We can see a prototype
   for this in the 'Leaf' case in 'numberTree' above:

      numberTree Leaf counter = (counter, Leaf)

   this returns 'Leaf', while simulating *not* mutating the
   counter. Let's capture this pattern as a function: -}

returnState :: a -> State a
returnState x s = (s, x)

{- So 'returnState "hello"' is a (simulation of) a mutating operation
   that does no mutation and returns "hello".

   Following the example of 'Maybe' from the last lecture, the next
   step is to tidy up the steps of 'numberTree' that simulate
   execution of steps one after the other, passing the value of the
   current state through. Let's look at the example again:

       let (counter0, l') = numberTree l counter
           .. continuation code using 'counter0' and l' ..

   We have the same pattern of "return a result and use it in the
   continuation" as we did for 'Maybe' and exceptions. Let's try
   writing down the a similar type: -}

andThen :: State a -> (a -> State b) -> State b
{- The idea is that we do a state mutating operation to get an 'a', then
   we use that 'a' to do another state mutating operation to get a
   'b'. The whole thing is a state mutating operation that returns a
   'b'.

   The implementation looks like: -}
andThen op k = \s ->
  let (s0, a) = op s
      (s1, b) = k a s0
  in (s1, b)

{- The implementation is very terse, but it states:

      to do operation 'op' with continuation 'k', starting from state
      value 's', first run 'op' on 's' to get the state 's0' and value
      'a'. Then run the continuation 'k' with the value 'a' and pass
      in the state value 's0'. This yields the final state 's1' and
      value 'b', which we return.

   We are not yet quite at the stage that we can rewrite 'numberTree'
   using 'returnState' and 'andThen', because we have no functions for
   accessing or updating the state. We do this with functions 'get'
   and 'put': -}

get :: State Int
get = \s -> (s,s)

put :: Int -> State ()
put s' = \s -> (s',())

{- The type of 'get' says that it is a state mutating operation that
   returns an 'Int'. The implementation says that it is the operation
   that does not actually mutate the state, but does return the
   current state.

   The type of 'put' says that it is a state mutating operation that
   takes an 'Int' and returns an 'Int'. It mutates the state by
   replacing it by its 'Int' argument, and returns the value '()',
   representing no useful information (the "unit type", written '()',
   has one constructor '()'. It is often used in these simulations of
   side effects to report that no value is returned, only that some
   side effect has been performed).

   We can now use 'returnState', 'andThen', 'get', and 'put' to
   rewrite 'numberTree' to hide all the "plumbing". The rewriting
   makes it all look very similar to 'lookupAll' from the last
   lecture: -}

numberTree_v2 :: Tree a -> State (Tree (a, Int))
numberTree_v2 Leaf =
  returnState Leaf
numberTree_v2 (Node l x r) =
  numberTree_v2 l `andThen` \l' ->
  get             `andThen` \i  ->
  put (i+1)       `andThen` \() ->
  numberTree_v2 r `andThen` \r' ->
  returnState (Node l' (x, i) r')

{- We can further simplify by defining our own state mutating operations
   that do exactly what we want to the state. For example, if we
   notice that the sequence "get then put incremented by one" happens
   a lot in programs (this is similar to the expression 'x++' in C and
   Java), then we can write it as another state mutating operation: -}

getAndIncrement :: State Int
getAndIncrement s = (s+1,s)

{- And use it to shorten 'numberTree' again: -}

numberTree_v3 :: Tree a -> State (Tree (a, Int))
numberTree_v3 Leaf = returnState Leaf
numberTree_v3 (Node l x r) =
  numberTree_v3 l `andThen` \l' ->
  getAndIncrement `andThen` \i  ->
  numberTree_v3 r `andThen` \r' ->
  returnState (Node l' (x, i) r')

{- We could have also written 'getAndIncrement' in terms of 'get',
   'put', 'andThen' and 'returnState': -}

getAndIncrement_v2 :: State Int
getAndIncrement_v2 =
  get       `andThen` \i ->
  put (i+1) `andThen` \() ->
  returnState i

{- Finally, how do we get from a state mutating operation back to the
   world of just values? How do we run the simulation? We have to
   provide an initial state. Since we are simulating state mutating
   operations by using functions, this is a case of applying the
   simulation of the state mutation to the initial value of the state,
   to get the final value and the result: -}

runState :: State a -> Int -> (Int,a)
runState t i = t i


{-    Part 6.4 : SIMULATING OUTPUT

   Another side effect often used in programs is printing: outputting
   stuff to the screen. Let's see how to simulate this in Haskell, and
   whether we can identify similar patterns as we did in exceptions
   and mutable state.

   We'll simulate printing by requiring that functions that do
   "printing" return a list of 'String's, representing the lines of
   output that they produce.

   For our example this time, we'll write a function that goes through
   a tree of 'Int's adding up all the numbers, while also printing out
   each number as it comes to it. The implementation looks a bit
   similar to the state mutating one above, except with the major
   differences that there is no initial state, and the way the final
   state is computed. -}

printAndSum :: Tree Int -> ([String], Int)
{- The type signature states what we said above: the function takes a
   'Tree Int' and returns a pair of lines of output, and the sum of
   the 'Int's in the tree. -}

printAndSum Leaf = ([], 0)
{- In the 'Leaf' case, we produce no lines of output ('[]') and the sum
   of a 'Leaf' is 0. -}

printAndSum (Node l x r) =
  let (o1, lsum) = printAndSum l
      o2         = [show x]
      (o3, rsum) = printAndSum r
  in (o1 ++ o2 ++ o3, lsum + x + rsum)
{- In the 'Node' case, we printAndSum the left subtree, getting output
   'o1' and sum 'lsum'. Then we use 'show' to turn 'x' into a
   'String', and call the line of output for this node 'o2'. Then we
   'printAndSum' the right subtree, calling the output 'o3' and the
   sum 'rsum'. Finally, we concatenate all the outputs, and add up the
   lsum, r, and rsum.

   Just as with the exception and mutable state examples, this is
   messy, because all the plumbing is exposed, and there is again
   great opportunity for messing up due to all the repetitive code.

   As with exceptions and mutable state, let's tidy this up by first
   identifying the core type used in our simulation of
   printing. Looking at the type of 'printAndSum' above, we see that
   the result is a pair of the list of lines printed, and the actual
   result. Let's turn this into a type definition: -}

type Printing a = ([String],a)

{- This definition says "An operation that does some printing and
   returns a value of type 'a' is simulated using a pair of a list of
   strings and the actual result".

   As before, we need a way to say that we want to return a value
   without doing any printing. We define 'returnPrinting' to do this: -}

returnPrinting :: a -> Printing a
returnPrinting x = ([], x)

{- The type of 'returnPrinting' says that it takes a value of type 'a'
   and returns a (simulation of) an operation that may do some
   printing and returns a value of type 'a'. The implementation says
   that it in fact does no printing (using the empty list of lines),
   and returns the value given as an argument.

   'returnPrinting' suffices to tidy up the 'Leaf' case of the
   'printAndSum' function. To tidy up the plumbing of printing, we
   look at a prototypical case of doing an operation with some
   printing that returns a result 'a', and then running another
   printing operation that requires 'a'. Finally, we concatenate the
   two lists of lines, and the final returned value. This will look
   like:

       let (o1, a) = op
           (o2, b) = k a
       in (o1 ++ o2, b)

   Let's turn this into a definition. As for mutable state and
   exceptions, we take an operation to do first, a continutation, and
   return an operation that does both. -}

andThenWithPrinting :: Printing a -> (a -> Printing b) -> Printing b
andThenWithPrinting op k =
  let (o1, a) = op
      (o2, b) = k a
  in (o1 ++ o2, b)

{- If we only have the ability to do no printing ('returnPrinting') and
   to sequence a printing operation and a continuation
   ('andThenWithPrinting'), then we won't actually do any printing! To
   fix this, we define a primitive printing operation, 'printLine'
   that takes a string to print and simulates printing it by returning
   a list of lines with only that string in it: -}

printLine :: String -> Printing ()
printLine s = ([s], ())

{- With this type definition and three functions, we can tidy up the
   definition of 'printAndSum' in a way that is very similar to the
   ways we tidied up the exceptions and mutable state examples before: -}

printAndSum_v2 :: Tree Int -> Printing Int
printAndSum_v2 Leaf =
  returnPrinting 0
printAndSum_v2 (Node l x r) =
  printAndSum_v2 l   `andThenWithPrinting` \lsum ->
  printLine (show x) `andThenWithPrinting` \() ->
  printAndSum_v2 r   `andThenWithPrinting` \rsum ->
  returnPrinting (lsum + x + rsum)



{-    Part 6.5 : WHAT'S THE COMMON PATTERN?

   We've now seen three examples of simulations of side effects:
   exceptions, mutable state, and printing. In the case of exceptions,
   we simulated them by using the 'Maybe' type. For mutable state, the
   simulation worked by passing the state through the function as an
   extra parameter. For printing, the simulation collected all the
   printed strings in a list.

   In each case, writing the code out explicitly was messy -- it is
   boring to write, and error prone due to the large amount of
   repetition. We were able to tidy up the examples by first writing a
   type synonym that captured the essence of the way we were
   simulating the side effect, and then by writing functions that
   helped us hide the plumbing. In each of the three cases, we had a
   function that simulated "doing nothing" while returning a value:

       return         :: a -> Maybe a
       returnState    :: a -> State a
       returnPrinting :: a -> Printing a

   And in each of the three cases, we had a function that simulated
   "sequencing" of a side effecting operation before a continuation:

       ifOK                :: Maybe a    -> (a -> Maybe b)    -> Maybe b
       andThen             :: State a    -> (a -> State b)    -> State b
       andThenWithPrinting :: Printing a -> (a -> Printing b) -> Printing b

   And then for each of the three cases, we had special functions
   peculiar to that kind of side effect. For exceptions, we had
   'failure' and 'catch'. For mutable state, we had 'get' and 'put',
   and for printing we had 'printLine'.

   But for all the cases we've seen so far, there was a common core:
   the "do nothing" operation, and the "do this and then do that"
   operation. In the next lecture, we'll define a type class that
   gives types that support this pair of operations a name "Monad",
   and investigate some more examples. -}
