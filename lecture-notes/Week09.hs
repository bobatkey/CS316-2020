{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week09 where

import Control.Concurrent (forkIO, MVar, newEmptyMVar, putMVar, takeMVar)
import Prelude hiding (mapM)
import Data.Traversable   (for)
import Network.HTTP       ( simpleHTTP
                          , getRequest
                          , getResponseBody
                          )
import Week08 (Parser, runParser, JSON (..), parseJSON)


{-   WEEK 9 : DATA DEPENDENCIES and APPLICATIVE FUNCTORS

   Since Week 07, we've been using the 'Monad' typeclass as a general
   interface for organising side effecting computations. Here is the
   'Monad' definition again (commented out because we are now using
   the 'official' Monad definition from the standard library: -}

{-
class Monad m where
  return :: a -> m a
  (>>=)  :: m a -> (a -> m b) -> m b
-}

{- The 'Monad' type class attempts to capture the idea that values of
   type 'm a' represent side effecting operations that result in
   values of type 'a'. To fulfil the 'Monad' interface, a type
   constructor 'm' must implement a 'return' function, that generates
   a "do nothing" operation, and a bind function '>>=', that sequences
   two operations.

   A key point about the '>>=' function is that it allows for the
   second operation to depend on the value returned by the first. This
   is because a function of type 'a -> m b' can decide what side
   effecting operation it wants to do by looking at the 'a' value. We
   say that there is a potential data dependency between the two
   computations.

   This week, we'll look at an alternative interface to side effecting
   computations that still allows computations to be sequenced, but
   disallows data dependencies between them. This interface will be
   called 'Applicative', after an old name for Functional Programming
   ("Applicative Programming", because it is based around the idea of
   applying functions).

   'Applicative's will be useful in two ways. First, they make some
   programs a bit nicer to write, making their structure as "function
   application with a bit extra" more apparent. Second, disallowing
   data dependencies means that it more obvious when certain
   operations can be run in parallel. This second advantage has been
   put to use in the Haxl library developed by Facebook. We will
   develop a toy version of Haxl in the last part of this week. -}

{-    Part 9.1 : Sequences of Actions

   To introduce the idea of Applicatives, we first look at a common
   pattern that arises when using monads. Often we will execute a
   sequence of actions, none of which depend on the results of the
   earlier ones, and then we apply a function to all the results at
   the end. For example, traversing a list, performing some action for
   every element is performed by the 'mapM' function we saw in Week
   07: -}

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f [] =
  return []
mapM f (x:xs) =
  do x'  <- f x
     xs' <- mapM f xs
     return (x':xs')

{- The analogous function for traversing 'Tree's, which was part of the
   tutorial questions for Week 07, looks similar: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

mapTreeM :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapTreeM f Leaf =
  return Leaf
mapTreeM f (Node l x r) =
  do l' <- mapTreeM f l
     x' <- f x
     r' <- mapTreeM f r
     return (Node l' x' r')

{- These functions both follow the pattern of:

       do a1 <- action1
          a2 <- action2
          a3 <- action3
          ...
          return (f a1 a2 a3 ...)

   We often have a sequence of actions to perform, none of which
   depend on the results of the previous action. The final result is
   returned by combining the 'a1', 'a2', 'a3', ... with some function
   'f'.

   Another example of this pattern occurring is with parser
   combinators. For example, to parse an JSON object field
   ('"fieldname" : <value>') we wrote code like:

       do fieldname <- parseStringLiteral
          _         <- parseLiteralChar ':'
          value     <- parseItem
          return (fieldname, value)

   which again consists of a sequence of actions that do not depend on
   each other's results, and a final operation to put together all the
   results.

   Since we see this pattern over and over again, it looks like it
   might be worth investigating ways of tidying it up. One way of
   doing this is to write a separate function for each number of
   actions we want to perform. Then we don't have to go to the bother
   of naming all the intermediate results, and can just use normal
   function application.

   When we have no actions, then we are just returning a single value,
   so we can write:

      return x

   When we have one action to perform, we can use a function 'lift1',
   which "lifts" a function of type 'a -> b' to take actions that
   produce 'a's to actions that produce 'b's: -}

lift1 :: Monad m => (a -> b) -> m a -> m b
lift1 f action1 =
  do a <- action1
     return (f a)

{- When we have two actions, we could write 'lift2': -}

lift2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2 f action1 action2 =
  do a <- action1
     b <- action2
     return (f a b)

{- When we have three actions, we could write 'lift3': -}

lift3 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3 f action1 action2 action3 =
  do a <- action1
     b <- action2
     c <- action3
     return (f a b c)

{- And so on for 'lift4', 'lift5', ...

   (notice how the 'lift1' function has a similar type signature to
   'mapPicture' from Exercise 2, and 'lift2' has a similar type
   signature to 'mapPicture2'.)

   We'd rather not have a special function for each number of
   arguments that we want to lift. Is there a way of defining a small
   number of functions that can be chained together to produce the
   same effect?

   Let's look at the way that function application works in Haskell in
   more detail. Back in Week 03, we learned that multi-argument
   functions in Haskell are actually single argument functions that
   take one argument and then return a function expecting the rest of
   the arguments. For example, if we have a function with the
   following type:

      addThreeNumbers :: Int -> Int -> Int -> Int

   and we apply it to one number, then we get back a function that
   takes two 'Int's and returns an 'Int':

      addThreeNumbers 1 :: Int -> Int -> Int

   and again if we apply it to another number, we get back a function
   that expects one 'Int' and returns an 'Int':

      addThreeNumbers 1 10 :: Int -> Int

   We could make this process more explicit if we had a way of marking
   where the function applications were happening. Let's define a
   function that does function application: -}

apply :: (a -> b) -> a -> b
apply f a = f a

{- So 'f `apply` a' is the the same as 'f a'. We can use it to delineate
   where function applications are happening.

   Written out more explicitly, applying the function
   'addThreeNumbers' to two arguments looks like this:

      (addThreeNumbers `apply` 1) `apply` 10

   Spelling this out, we have:

     a) addThreeNumbers :: Int -> (Int -> (Int -> Int))

     b) addThreeNumbers `apply` 1 :: Int -> (Int -> Int)

     c) (addThreeNumbers `apply` 1) `apply` 10 :: Int -> Int

   So, 'apply' takes a pure function 'a -> b' and a pure value 'a' and
   returns something of type 'b'.

   Returning to our original problem, we want to lift function
   application up to actions in some monad. Could we do this if we had
   something similar to 'apply', but that worked on functions and
   arguments that were the result of actions. Let's have a go, and
   call that function 'mapply': -}

mapply :: Monad m => m (a -> b) -> m a -> m b
mapply mf ma =
  do f <- mf
     a <- ma
     return (f a)

{- So 'mapply' takes an action that will return a function, and an
   action that will return a value, and returns an action that runs
   the first two in sequence and then applies the function to the
   value.

   The difference between this function and the 'lift' functions we
   wrote before is that the function argument is also the result of
   some action. This will allow us to chain together several uses of
   'mapply' to reach any number of arguments.

   (Notice that 'mapply' has a similar type signature to the
   'pictureApply' function from Exercise 2.)

   We can see how 'mapply' works by using it to write the 'lift'
   functions from earlier: -}

lift1_v2 :: Monad m => (a -> b) -> m a -> m b
lift1_v2 f action1 =
  return f `mapply` action1

lift2_v2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
lift2_v2 f action1 action2 =
  return f `mapply` action1 `mapply` action2

lift3_v2 :: Monad m => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
lift3_v2 f action1 action2 action3 =
  return f `mapply` action1 `mapply` action2 `mapply` action3

{- Each one works in the same way: it uses 'return' to lift the function
   'f' up into the monad, and then uses 'mapply' to repeatedly apply
   it to arguments. 'mapply' is like a version of function application
   that allows side effects to happen at the same time.

   We can now rewrite 'mapM' and 'mapTreeM' to use 'mapply' instead,
   making their structure a bit more explicit. They both work by
   applying the appropriate constructor ('(:)' for lists, 'Node' for
   trees) to the results of processing the sub-lists/trees and
   data. Using 'mapply' instead of normal function application allows
   the side effects to be processed correctly: -}

mapM_v2 :: Monad m => (a -> m b) -> [a] -> m [b]
mapM_v2 f [] =
  return []
mapM_v2 f (x:xs) =
  return (:) `mapply` f x `mapply` mapM_v2 f xs

mapTreeM_v2 :: Monad m => (a -> m b) -> Tree a -> m (Tree b)
mapTreeM_v2 f Leaf =
  return Leaf
mapTreeM_v2 f (Node l x r) =
  return Node `mapply` mapTreeM_v2 f l `mapply` f x `mapply` mapTreeM_v2 f r

{- In both cases, the idea is that the function looks the same as the
   normal 'map' / 'mapTree', except that we have to put in some extra
   noise to handle the side effects. -}


{-     Part 9.2 : Applicative, a New Typeclass

   Often, when we want to do programming with side effects, it
   suffices to use only 'return' and 'mapply', like in 'mapM' and
   'mapTreeM'. This was first observed by Conor McBride and Ross
   Paterson in their paper "Applicative Programming with Effects":

      http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf

   They proposed an interface to side effects that is based around
   only a 'return'-like function and a 'mapply'-like function. The
   names they proposed were 'pure' and '<*>' (pronounced "apply"). The
   type class that puts these two together is called 'Applicative'
   (after "Applicative Programming", as mentioned above): -}

{-
class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}

{- The 'Functor f =>' bit means that every 'Applicative' implementation
   must also have a 'Functor' implementation, where 'Functor' is the
   type class for container-like things that we introduced in Week 05: -}

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b
-}

{- We can always define an 'fmap' for every 'Applicative', because it is
   the same as the 'lift1' function we saw above. So we could define:

      fmap f action = pure f <*> action

   But the design of the standard library allows us to write a custom
   implementation of 'fmap' if we need to, in case it might be more
   efficient.

   As we have seen with the definition of 'mapply', every 'Monad' is
   an 'Applicative' by defining 'pure' to be 'return' and '<*>' to be
   'mapply'. The official definition of 'Monad' in the standard
   library is not quite like how we defined it above. It requires an
   implementation of 'Applicative', just as 'Applicative' requires an
   implementation of 'Functor':

      class Applicative m => Monad m where
        return :: a -> m a
        (>>=)  :: m a -> (a -> m b) -> m b

   There is also a convention that if a type constructor has a 'Monad'
   interface, then the 'Applicative' interface should act as if 'pure'
   is the same as 'return', and '<*>' is the same as 'mapply'.

   This convention means that there are useful 'Applicative's that do
   not have matching 'Monad' implementations. For example, the type of
   triples: -}

data Triple a = MkTriple a a a
  deriving Show

{- We can think of a value of 'Triple a' as like the pictures from
   Exercise 2, except with only three points.

   Defining a 'Functor' implementation is a matter of applying a
   single function to every point (like 'mapPicture'): -}

instance Functor Triple where
  fmap f (MkTriple a1 a2 a3) =
    MkTriple (f a1) (f a2) (f a3)

{- Defining an 'Applicative' implementation has two operations: 'pure'
   takes a single value and puts it in every point, and '<*>' takes
   the function at each point and applies to the value at that point: -}

instance Applicative Triple where
  pure :: a -> Triple a
  pure a =
    MkTriple a a a

  (<*>) :: Triple (a -> b) -> Triple a -> Triple b
  MkTriple f1 f2 f3 <*> MkTriple a1 a2 a3 =
    MkTriple (f1 a1) (f2 a2) (f3 a3)

{- EXERCISE: there is a possible monad implementation for Triple, but it
      doesn't have the property that the applicative implementation
      agrees with it. Write the monad implementation, and show that
      'mapply' for it doesn't give the same answer as the '<*>'
      defined here. -}


{- We have seen that the 'Applicative' interface is all we need to
   define functions that perform traversals of data structures like
   lists and trees. Writing the same functions using the 'Applicative'
   interface is even more concise: -}

mapA :: Applicative f => (a -> f b) -> [a] -> f [b]
mapA f []     = pure []
mapA f (x:xs) = pure (:) <*> f x <*> mapA f xs

mapTreeA :: Applicative f => (a -> f b) -> Tree a -> f (Tree b)
mapTreeA f Leaf         = pure Leaf
mapTreeA f (Node l x r) = pure Node <*> mapTreeA f l <*> f x <*> mapTreeA f r

{- Also, these functions now work for any 'Applicative', not just any
   'Monad'. As we saw with the 'Triple' example, there are potentially
   useful types that are 'Applicative', but not 'Monad'. The 'Picture'
   type from Exercise 2 is another example.

   'Applicatives' are also useful for parsing. The example parser we
   saw above can now be written as:

       pure (\fieldname _ value -> (fieldname, value))
          <*> parseStringLiteral
          <*> parseLiteralChar ':'
          <*> parseItem

   which makes the separation of the "things to be recognised" and the
   "way of combining them" more clear.

   It is also possible to give an alternative implementation of the
   'Parser' type from Week 08 that exploits the denial of data
   dependency by the 'Applicative' interface to make parsing more
   efficient by having a fixed grammar that can be optimised. We'll
   not look into this in this course. Instead, we'll look at a way
   that the 'Applicative' interface can be used to make data retrieval
   operations happen in parallel. -}



{-       Part 9.3 : Data Dependencies and Parallelism

   As we mentioned above, the key difference between the 'Monad' and
   'Applicative' interfaces is whether or not the second operation to
   be performed can depend on the result of the first. In the 'Monad'
   "bind" function:

      (>>=) :: m a -> (a -> m b) -> m b

   the second operation is a function 'a -> m b', which can inspect
   the 'a' value and return any operation it likes. In the
   'Applicative' "apply" function:

      (<*>) :: f (a -> b) -> f a -> f b

   The two operations are fixed from the start, only the final 'b'
   value can depend on the results of the operations. This lack of
   data dependency can be exploited to reveal opportunities for
   parallelism: if the two operations given to '<*>' can't depend on
   each other, then they could be executed in parallel.

   Facebook's Haxl library is an implementation of this idea. Haxl is
   used within Facebook to implement high performance data fetching
   tasks. Generating a webpage often requires fetching data from a
   multitude of independent backend data services. Performing these
   fetches in parallel can drastically lower the response time of a
   website.

   Haxl's code is available on GitHub:

      https://github.com/facebook/Haxl

   and there is paper describing the idea, evaluating it on some
   benchmarks:

      http://simonmar.github.io/bib/papers/haxl-icfp14.pdf

   The paper is very readable and is at about the level of Haskell
   that you're at in this course. The start of the paper describes a
   toy version of Haxl to get the idea across. Here I'll describe a
   slightly different toy version that is just enough not-toy to be
   able to implement a real concurrent fetching implementation for it
   below in Part 9.6.

   For our toy version, we'll just represent requests to and responses
   from backend data services as 'String's: -}

type Request = String
type Response = String

{- The core data structure is 'Fetch', which represents a sequence of
   batches of requests to be made: -}

data Fetch a
  = Done a
  | Blocked [Request] ([Response] -> Fetch a)

{- A 'Fetch a' value is either:

    1. 'Done x', representing a job that has completed and returned
       the value 'x'; or

    2. 'Blocked requests k' representing a job that is blocked on a
       list of 'requests'. Once these requests have been fulfilled,
       the continuation 'k' can be applied to the responses, yielding
       the rest of the job.

   Notice that 'Fetch' is very similar to the 'Process' type in
   Exercise 2. There are two key differences:

    1. The 'Process' type had separate 'Input' and 'Output'
       constructors, which have been merged here -- every 'Output' is
       immediately followed by an 'Input'.

    2. The "outputs" and "inputs" are lists of requests and responses,
       because we want to record which requests can be run in
       parallel.

   To make individual requests, 'Fetch' has a primitive operation
   'makeRequest', which takes a 'Request' and returns a 'Fetch' job
   that will yield a 'Response': -}

makeRequest :: Request -> Fetch Response
makeRequest request =
  Blocked [request] (\[response] -> Done response)

{- To see how 'Fetch' works we'll define a custom 'Show' implementation
   for it. We can't in general print out functions, but we can print
   out the first thing that a 'Fetch' job will do. Either it is
   'Done', or it is 'Blocked' on a list of requests: -}

instance Show a => Show (Fetch a) where
  show (Done a)             = "(Done " ++ show a ++ ")"
  show (Blocked requests _) = "(Blocked " ++ show requests ++ " <waiting>)"

{- For example, 'makeRequest "A"' generates a 'Fetch' job that is
   blocked on the request "A":

      > makeRequest "A"
      (Blocked ["A"] <waiting>)

   We'll now define a 'Monad' implementation for 'Fetch'. The 'return'
   generates 'Fetch' jobs that are already 'Done'. The "bind" ('>>=')
   sequences 'Fetch' jobs one after the other, in a similar way to the
   'sequ' function in Exercise 2 sequences 'Process'es one after the
   other. -}

instance Monad Fetch where
  return :: a -> Fetch a
  return x = Done x

  (>>=) :: Fetch a -> (a -> Fetch b) -> Fetch b
  Done a >>= f =
    f a
  Blocked requests k >>= f =
    Blocked requests (\responses -> k responses >>= f)

{- For example, if we write a 'Fetch' job that makes two requests in
   sequence, like this: -}

sequentialJob :: Fetch (Response, Response)
sequentialJob =
  do a <- makeRequest "A"
     b <- makeRequest "B"
     return (a, b)

{- Then if we print this out, we can see that the 'Fetch' job is first
   'Blocked' on the request for "A", and will not proceed until this
   request is fulfilled:

      > sequentialJob
      (Blocked ["A"] <waiting>)

   If we could print out functions, then the whole 'Fetch' job would
   look like this, showing that the "A" request is scheduled to happen
   before the "B" request.

      Blocked ["A"] (\[a] -> Blocked ["B"] (\[b] -> Done (a,b)))

   An implementation of the 'Monad' interface has no option but to
   sequence the jobs, because it cannot access the second 'Fetch' job
   until it knows the value returned by the first, due to the
   potential data dependency.

   In contrast, the 'Applicative' interface has access to both of the
   'Fetch' jobs it is given, and can merge them when it sees fit,
   allowing for multiple requests with no data dependency to be
   parallelised. Here is the implementation of 'Applicative' for
   'Fetch': -}

instance Applicative Fetch where
  pure :: a -> Fetch a
  pure x = Done x
{- The 'pure' implementation is the same as the 'return' in the 'Monad'
   implementation. -}

{- The 'apply' ('<*>') implementation merges the two 'Fetch' jobs: -}
  (<*>) :: Fetch (a -> b) -> Fetch a -> Fetch b
  Done f               <*> Done a =
    Done (f a)
{- If both are done, then it applies the function from the first to the
   value from the second. -}

  Done f               <*> Blocked requests k =
    Blocked requests (\responses -> Done f <*> k responses)

  Blocked requests k   <*> Done a =
    Blocked requests (\responses -> k responses <*> Done a)
{- If one is done and the other is blocked, then the result is blocked
   on the same requests, and 'apply' is called recursively to apply
   the 'Fetch' job after the response has been received. -}

  Blocked requests1 k1 <*> Blocked requests2 k2 =
    Blocked (requests1 ++ requests2)
      (\responses ->
         let responses1 = take (length requests1) responses
             responses2 = drop (length requests1) responses
         in
         k1 responses1 <*> k2 responses2)
{- If both are Blocked, then we concatenate the lists of requests,
   creating a single 'Blocked' constructor that contains several
   requests that can be executed in parallel. The continuation then
   has to separate the responses and pass them on to the two
   continuations. -}

{- Since every Applicative must also have a 'Functor' implementation, we
   just give the simplest 'fmap' implementation in terms of 'pure' and
   '<*>' as we described above: -}

instance Functor Fetch where
  fmap :: (a -> b) -> Fetch a -> Fetch b
  fmap f fetch = pure f <*> fetch

{- To see the difference this 'Applicative' instance makes, we rewrite
   the 'sequentialJob' 'Fetch' to use the 'Applicative' interface
   instead: -}

parallelJob :: Fetch (Response, Response)
parallelJob =
  pure (\a b -> (a, b)) <*> makeRequest "A" <*> makeRequest "B"

{- If we print this out, we can see that it has merged the two requests
   "A" and "B" into one 'Blocked', meaning that something executing
   these 'Fetch' jobs can executed them in parallel:

       > parallelJob
       (Blocked ["A","B"] <waiting>)

   If we could print out functions, then 'parallelJob' would look
   like this:

       Blocked ["A","B"] (\[a,b] -> Done (a,b))

   Showing that there is only one 'Blocked', with two jobs to run in
   parallel. Once they have both finished, the task is 'Done'.

   Below, we write a real parallel executor that executes the requests
   in parallel, using the concurrency features of Haskell. -}


{-     Part 9.4 : Concurrency and Communication

   To complete our exploration of using Applicatives for making
   parallel requests, we will make an implementation of 'Fetch' that
   uses real concurrency to make requests to an HTTP endpoint. We
   first need a quick overview of Haskell's support for concurrency.

   For more information about concurrent and parallel programming in
   Haskell, I strongly recommend this book by one of the key
   developers of GHC, who now works at Facebook:

      Simon Marlow. "Parallel and Concurrent Programming in
      Haskell". 2013. O'Reilly Media. ISBN 9781449335946.
      https://simonmar.github.io/pages/pcph.html
      https://www.oreilly.com/library/view/parallel-and-concurrent/9781449335939/

   The basic primitive provided by Haskell for running two parts of a
   program concurrently is the ability to "fork" off a background task
   while continuing to run in the foreground. The individual tasks are
   called "threads" ("of execution").

   Here is an example: -}

concurrentMessages :: IO ()
concurrentMessages =
  do forkIO (do putStrLn "Hello from the background thread!")
     putStrLn "Hello from the foreground thread!"

{- If we run this 'IO' action in GHCi, then we can see that the
   execution of the foreground and background threads are interleaved:

      > concurrentMessages
      HelHleol lfor ofmr otmh et hfeo rbeagcrkogurnodu ntdh rteharde!a
      d!

   Running the same action again may get different interleavings,
   dependening on how the Haskell system and/or the operating system
   decide to schedule the two threads.

   'forkIO' has the following type signature:

       forkIO :: IO () -> IO ThreadId

   meaning that it takes an 'IO ()' action to be run in the
   background, and returns an IO action that has the effect of forking
   off the background thread and returns a 'ThreadId'. 'ThreadId's are
   mainly useful for terminating threads that are no longer needed
   (though, in general, this is a risky operation). -}

{- Once we have a way to make multiple threads, we need a way to
   communicate between them. Several ways of doing this are popular:

   - Using shared memory. Multiple threads communicate by reading and
     writing to shared memory, as if there were a global notice board
     that they all have access to. Of course, to prevent data
     corruption, care must be taken to synchronise accesses by means
     of locks or other methods. Shared memory is how low-level
     concurrency in languages like Java operates, though Java also
     offers a very wide range of higher level primitives on top.

   - Message passing. Multiple threads communicate by sending messages
     to each other. This avoids the some of the synchronisation issues
     inherent to shared memory, but introduces problems when messages
     may be lost, duplicated, or arrive in unexpected orders. Message
     passing is the only option for distributed computing across a
     network where there is no memory to share, but it has also been
     effectively used in languages such as Erlang (used for, e.g.,
     WhatsApp).

   Haskell uses an interesting take on shared memory that avoids some
   of the issues with synchronisation, and has some of . The basic primitive for
   inter-thread communication is the 'MVar': a special kind of mutable
   variable that has the following properties:

   - MVars start off empty.

   - Writing a new value can only happen if it is empty. If it is not
     empty, the thread will block (wait) until it is empty.

   - Reading the value of an MVar empties the MVar. If the MVar is
     empty, reading blocks until a value is placed in it.

   A useful intuition is to think of MVars as a 'mailbox' that we can
   place individual messages into. If a sender tries to place a
   message into a mailbox that is already full, then they must
   wait. Simialrly, if a reader tries to take a message from an empty
   mailbox, then they must wait. Another way to think about MVars is
   as message passing channels with a capacity of 1.

   The Haskell inteface for MVars consists of the following type and
   functions:

   type MVar a                       -- an abstract type of mailboxes that can hold values of type 'a'

   newEmptyMVar :: IO (MVar a)       -- create a new empty MVar

   putMVar :: MVar a -> a -> IO ()   -- place a value into an MVar, blocking until it is empty

   takeMVar :: MVar a -> IO a        -- take a value from an MVar, blocking until there is a value to take

   Let's see 'MVar's in action. The following function takes an MVar
   and forks off a thread that waits for a message to be placed into
   the MVar. When it receives the message, it prints it out: -}

spawnReceiver :: MVar String -> IO ()
spawnReceiver mvar =
  do forkIO (do msg <- takeMVar mvar
                putStrLn ("Message received: " ++ msg))
     return ()

{- To use this function in GHCi, we first need to create an MVar, which
   we call 'chan' (short for 'channel'):

       *Week09> chan <- (newEmptyMVar :: IO (MVar String))
       *Week09> :t chan
       chan :: MVar String

   We then intialise the receiver with this channel, which will fork
   off a background thread waiting for a message to be sent:

       *Week09> spawnReceiver chan

   To send it a message, we put a string into the 'chan' MVar. Note
   how the printing of the message by the background thread is
   interleaved with the printing of the prompt in the foreground:

       *Week09> putMVar chan "Hello!"
       Messa*Week09> ge received: Hello!

   The background thread has now terminated, so if we put another
   string into 'chan', nothing happens:

       *Week09> putMVar chan "Hello!"

   The MVar is now full, so if we put one more thing into it, we are
   blocked, and have to press Ctrl-C to interrupt it:

       *Week09> putMVar chan "Hello!"
       ^CInterrupted.
       *Week09>

   See Marlow's book for how to build channels and other abstractions on
   top of MVars. -}


{-     Part 9.5 : A Logging 'Object'

   We can use 'MVar's to write a simple concurrent "object" that sits
   in the background listening for messages. It waits for messages to
   be put into an 'MVar' that is shared between itself and its
   clients. Upon receiving a message, it does whatever it wants,
   updates its internal state, and then continues to listen for
   messages.

   As an example, we'll write a simple Logger object. It listens for
   messages telling it what to log and outputs them to the screen,
   keeping a count of the number of messages logged so far. When it
   receives a message telling it to stop, it stops itself from
   running and outputs a final message.

   We first make a type for the kinds of message that we can send to
   the Logger: -}

data LogCommand
  = LogMessage String
  | LogStop (MVar ())

{- There are two kinds of message:

   - 'LogMessage msg' which tells the logger to log a string message

   - 'LogStop mvar' which tells the logger to stop, and passes in an
     (empty) 'MVar' that the logger uses to signal that it has
     stopped.

   A logger itself is represented as the mailbox we use to talk to it: -}

type Logger = MVar LogCommand

{- The logger implementation itself is a recursive function that waits
   for a message to appear in its mailbox (using
   'takeMVar'). Depending on the message, it then either outputs a log
   message and calls itself to process more messages, or outputs 'LOG
   STOPPED' and tells the caller it has stopped. -}

logger :: Logger -> Int -> IO ()
logger loggerVar counter =
  do cmd <- takeMVar loggerVar
     case cmd of
       LogMessage msg ->
         do putStrLn ("LOG(" ++ show counter ++ "): " ++ msg)
            logger loggerVar (counter+1)
       LogStop resp ->
         do putStrLn "LOG STOPPED"
            putMVar resp ()

{- To make a Logger, we create a new MVar to act as the mailbox between
   clients and the logger, and then fork off a thread so the logger
   can run in the background: -}

makeLogger :: IO Logger
makeLogger =
  do m <- newEmptyMVar
     forkIO (logger m 0)
     return m

{- The following two functions wrap the sending of messages to a
   background logger object in an easier to use interface: -}

logMessage :: Logger -> String -> IO ()
logMessage loggerVar msg =
  do putMVar loggerVar (LogMessage msg)

logStop :: Logger -> IO ()
logStop loggerVar =
  do resp <- newEmptyMVar
     putMVar loggerVar (LogStop resp)
     ()   <- takeMVar resp
     return ()

{- Here is a demonstration of its use:

       *Week09> log <- makeLogger
       *Week09> log `logMessage` "Hello"
       LO*Week09> G(0): Hello

       *Week09> log `logMessage` "World"
       LOG(1*Week09> ): World

       *Week09> logStop log
       LOG STOPPED

   As with the 'spawnReceiver' example above, the printing of the
   prompt has been interleaved with the printing of the log message. -}



{-     Part 9.6 Executing Requests Concurrently

   In Part 9.3 we saw how a toy version of Facebook's Haxl library
   uses the 'Applicative' interface to construct sequences of requests
   to be execute in parallel. We can now use the concurrent features
   demonstrated above to actually execute requests concurrently.

   First, we write a function that actually makes an HTTP request to
   some URL. We use the 'HTTP' library to do this (
   https://hackage.haskell.org/package/HTTP ). This is quite a basic
   library for doing HTTP requests -- it does not support HTTPS for
   example -- but it is easy to use and will suffice for this example.

   As well as making the actual request and reading the response back
   into a string, we also log what it is going on with an instance of
   the Logger object we wrote above: -}

doRequest :: Logger -> Request -> IO Response
doRequest log url =
  do log `logMessage` ("Requesting " ++ url)
     httpResp <- simpleHTTP (getRequest url)
     body <- getResponseBody httpResp
     log `logMessage` ("Request " ++ url ++ " finished")
     return body

{- To do requests in parallel, we write a function that works a bit like
   'mapM' except that it forks off a thread for each element in the
   list. It is worth going through this function to work out how MVars
   are used to collect up all the results of the forked off jobs: -}

mapMFork :: (a -> IO b) -> [a] -> IO [b]
mapMFork f xs =
  do mvars <- for xs (\a -> do mvar <- newEmptyMVar
                               forkIO (do b <- f a
                                          putMVar mvar b)
                               return mvar)
     results <- mapM takeMVar mvars
     return results

{- The 'runFetch' function now executes the commands in a 'Fetch'
   value. When the 'Fetch' value is 'Done', we stop and return the
   result. When the value is 'Blocked' with a sequence of requests to
   make, we do all those requests in parallel using 'mapMFork' and
   then execute the rest of the 'Fetch' value using the responses: -}

runFetch :: Logger -> Fetch a -> IO a
runFetch log (Done a) =
  do return a
runFetch log (Blocked requests k) =
  do responses <- mapMFork (doRequest log) requests
     runFetch log (k responses)

{- To execute a full 'Fetch a' value, we first need to initialise a
   logger to log progress to, run all the jobs, and then stop the
   logger: -}

runFetchWithLogger :: Fetch a -> IO a
runFetchWithLogger fetchJob =
  do logger <- makeLogger
     result <- runFetch logger fetchJob
     logStop logger
     return result

{- Here's a small example fetching the content of the page from
   http://www.example.com/:

      *Week09> runFetchWithLogger (makeRequest "http://www.example.com/")
      LOG(0): Requesting http://www.example.com/
      LOG(1): Request http://www.example.com/ finished
      LOG STOPPED
      "<!doctype html>\n<html>\n<head>\n ...."

   Now we'll make a little toy 'Fetch' job to request some JSON data
   from a HTTP endpoint. The endpoint we'll use for demonstration
   purposes is http://jsonplaceholder.typicode.com which offers a
   simple read-only REST interface to some JSON data. For example,
   requesting the URL:

       http://jsonplaceholder.typicode.com/todos/12

   returns the JSON:

      {
        "userId": 1,
        "id": 12,
        "title": "ipsa repellendus fugit nisi",
        "completed": true
      }

   and we can change the id number '12' to get other fake todo list
   items.

   We make a wrapper for 'makeRequest' to fetch some JSON data as a
   'String', and then uses the 'parseJSON' parser we wrote in Week 08
   to parse the response. If it fails to parse, it returns 'Nothing': -}

requestJSON :: Request -> Fetch (Maybe JSON)
requestJSON request =
  do response <- makeRequest request
     case runParser parseJSON response of
       Just ("", json) -> return (Just json)
       _               -> return Nothing

{- We'll also need some helper functions for extracting data from 'JSON'
   values. The first extracts the value associated with a named field
   in a JSON 'Object'. If the field doesn't exist, or the JSON value
   isn't an object, it returns 'Nothing': -}

getField :: String -> JSON -> Maybe JSON
getField fieldName (Object fields) = lookup fieldName fields
getField _               _         = Nothing

{- The second extracts the actual 'String' from a 'JSON' value holding a
   string. If the 'JSON' value isn't a string, it returns 'Nothing': -}

getString :: JSON -> Maybe String
getString (String str) = Just str
getString _            = Nothing

{- Now we can write a custom 'Fetch' job that takes an ID of a Todo, and
   extracts the title of the corresponding Todo item: -}

titleOfTodoId :: Int -> Fetch (Maybe String)
titleOfTodoId id =
  do let url = "http://jsonplaceholder.typicode.com/todos/" ++ show id
     json <- requestJSON url
     return (json >>= getField "title" >>= getString)

{- Now we can make two 'Fetch' jobs to fetch the titles of three todo
   items from the server. The first one uses the Monad interface to
   request each of them in sequence: -}

fetchTitlesSequential :: Fetch (Maybe String, Maybe String, Maybe String)
fetchTitlesSequential =
  do title1 <- titleOfTodoId 1
     title2 <- titleOfTodoId 12
     title3 <- titleOfTodoId 123
     return (title1, title2, title3)

{- The second uses the Applicative interface to request them all in
   parallel: -}

fetchTitlesParallel :: Fetch (Maybe String, Maybe String, Maybe String)
fetchTitlesParallel =
  pure (,,) <*> titleOfTodoId 1 <*> titleOfTodoId 12 <*> titleOfTodoId 123

{- We can see the difference when we run them in GHCi. First the
   sequential one:

       *Week09> runFetchWithLogger fetchTitlesSequential
       LOG(0): Requesting http://jsonplaceholder.typicode.com/todos/1
       LOG(1): Request http://jsonplaceholder.typicode.com/todos/1 finished
       LOG(2): Requesting http://jsonplaceholder.typicode.com/todos/12
       LOG(3): Request http://jsonplaceholder.typicode.com/todos/12 finished
       LOG(4): Requesting http://jsonplaceholder.typicode.com/todos/123
       LOG(5): Request http://jsonplaceholder.typicode.com/todos/123 finished
       LOG STOPPED
       (Just "delectus aut autem",Just "ipsa repellendus fugit nisi",Just "esse et quis iste est earum aut impedit")

   Looking at the log, we can see that each request waits for its
   response before doing the next request. In contrast, the parallel
   one fires off all the requests at once and then waits for the
   responses to come back. If the server can handle multiple requests
   in parallel, then this approach is much quicker than the sequential
   one:

       *Week09> runFetchWithLogger fetchTitlesParallel
       LOG(0): Requesting http://jsonplaceholder.typicode.com/todos/1
       LOG(1): Requesting http://jsonplaceholder.typicode.com/todos/12
       LOG(2): Requesting http://jsonplaceholder.typicode.com/todos/123
       LOG(3): Request http://jsonplaceholder.typicode.com/todos/12 finished
       LOG(4): Request http://jsonplaceholder.typicode.com/todos/1 finished
       LOG(5): Request http://jsonplaceholder.typicode.com/todos/123 finished
       LOG STOPPED
       (Just "delectus aut autem",Just "ipsa repellendus fugit nisi",Just "esse et quis iste est earum aut impedit")

   It is worth reading the paper describing the full Haxl system to
   get an idea how this technique scales up to handling different
   kinds of requests, and how it handles errors:

      http://simonmar.github.io/bib/papers/haxl-icfp14.pdf

   The code for Haxl is available on GitHub:

      https://github.com/facebook/Haxl

   The following blog post describes using it to fight spam on Facebook:

      https://engineering.fb.com/2015/06/26/security/fighting-spam-with-haskell/

   And there is a talk by Simon Marlow describing it:

      https://www.youtube.com/watch?v=sT6VJkkhy0o
-}
