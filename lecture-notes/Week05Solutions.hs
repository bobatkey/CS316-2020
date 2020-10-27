{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Week05 where

import Prelude hiding (Left, Right, Semigroup (..), Foldable (..), Functor (..), Monoid (..), Maybe (..))
import Data.Char

{-     WEEK 5 : CLASSES OF TYPES

   Haskell takes types very seriously. Every program must have a type,
   and Haskell will refuse to run a program if it cannot check that a
   program is consistent in the way that it uses types. Sometimes this
   is annoying. There are programs that would work fine even though
   the type checker rejects them. For example,

      one :: Int
      one = if True then 1 else "one"

   Because 'if True' always evaluates to the 'then' part, the result
   of this program will always be an 'Int', so the type declaration
   'one :: Int' is correct. However, the Haskell type checker rejects
   this, because the 'else' part returns a string.

   But why would anyone write such a convoluted definition instead of
   writing 'one = 1'? In general, while types can be annoying, they
   can also be very useful in communicating to other programmers, and
   to the compiler, what we expect the shapes of our data are. The
   basic philosophy of this course is that if you know something
   interesting about your program and its data, it is better to let
   the machine know too. Letting the machine in on your thoughts can
   help it write and maintain the program. with you.

   So what are types? Roughly speaking, types are sets of values, but
   Haskell has several ways of defining types which have different
   properties. -}


{-    Part 5.1 : TYPE SYNONYMS AND DATA TYPES

   It is sometimes useful to give new names to existing types. We
   might do this to for documentation purposes, or to avoid writing
   out a long type definition over and over again.

   We can give a new name to an existing type by making a declaration
   like so: -}

type Metres = Double

{- This defines 'Metres' as a synonym for the type 'Double', the
   built-in type of double precision floating point values. Now if we
   are writing some definitions where we intend the numbers to be
   interpreted as metres, then we can use 'Metres' as the type,
   instead of 'Double'. For example: -}

distanceToMoon :: Metres
distanceToMoon = 384402000
   -- https://en.wikipedia.org/wiki/Lunar_distance_(astronomy)

{- With the definition above, the name 'Metres' is just an alternative
   word for 'Double'. Wherever we can use a 'Double', we can also use
   'Metres'. For example, if we define a simple function on Doubles: -}

add :: Double -> Double -> Double
add x y = x + y

{- Then we can use it on 'Metres' with no complaints from the type
   checker, because it sees 'Metres' and 'Double' as the same type. -}

twiceDistanceToMoon :: Metres
twiceDistanceToMoon = add distanceToMoon distanceToMoon

{- Since, as far as the type checker is concerned, 'Metres' is exactly
   the same type as 'Double', using type synonyms like this is only
   for documentation purposes. There is nothing stopping us from
   making definitions that are nonsensical, like adding Metres and
   Seconds: -}

type Seconds = Double

secondsInAMinute :: Seconds
secondsInAMinute = 60

nonsense :: Double
nonsense = add secondsInAMinute distanceToMoon

{- To make Haskell distinguish between 'Metres' and 'Seconds', we will
   use a feature called 'newtype', described below. Nevertheless,
   giving new names, like 'Metres' and 'Seconds' to existing types can
   sometimes be a lightweight way of making the type signatures of
   functions easier to read.

   The second reason to give names to existing types is to give more
   meaningful names to complicated types, so that the type signatures
   of our functions are not littered with notation. For instance, if
   we wanted to use pairs of 'Int's as positions often in a program,
   we might make the following definition, defining the name
   'Position' to stand for the type '(Int,Int)': -}

type Position = (Int,Int)

{- Any pair of 'Int's can be used wherever a 'Position' is expected,
   making some definitions a bit more readable: -}

origin :: Position
origin = (0,0)

{- We can use type synonyms again when defining new type names. For
   instance, a "transformation" is a function that takes 'Position's
   to 'Position's. We can write this knowledge down as another type
   synonym declaration: -}

type Transformation = Position -> Position

{- Now when we write down some transformations, we can give a more
   meaningful type name than the original '(Int,Int) ->
   (Int,Int)'. For example: -}

goUp :: Transformation
goUp (x,y) = (x,y+1)

{- The type of 'goUp' is 'Transformation', which expands to 'Position ->
   Position', which expands to '(Int,Int) -> (Int,Int)'.

   Often, 'hiding' type definitions like this can improve code
   readability. It can also make code easier to modify. If we decided
   to change the representation of 'Position's to pairs of 'Double's,
   then we would just change the 'type Position = ...' definition and
   fix all the resulting type errors.

   However, it is not always the case that using type synonyms
   improves readability. Using the name 'Transformation' hides the
   fact that transformations are "really" functions, potentially
   obscuring the fact that they can be applied to 'Position's to
   transform them. Whether or not to use a type synonym depends on
   what you want to emphasise -- the underlying implementation, or the
   higher level idea. -}

{- Type synonyms can also take parameters, similar to how functions
   do. For example, -}

type Pair a = (a,a)

{- This defines a type /operator/ 'Pair', that when applied to another
   type 'X', stands for the type '(X,X)'. For example, we can define
   the type 'Position' again using 'Pair' like so: -}

type Position' = Pair Int

{- Now 'Pair Int' is equal to '(Int,Int)', which is equal to 'Position'
   as we defined above. -}

{- Type synonyms give new names to existing types, but don't generate
   any new types or any new kinds of value. To generate new types, and
   the data values that they contain, we use 'data' declarations. We
   have already seen some examples of 'data' declarations in the
   'Direction' and 'List' types, 'Maybe', and 'Tree'. Defining new
   datatypes using 'data' is the main way in Haskell of describing the
   kinds of data our programs manipulate.

   The simplest way to use 'data' is to define enumeration
   types. These are similar to "enum" types in Java:

      https://docs.oracle.com/javase/tutorial/java/javaOO/enum.html

   A standard example is days of the week: -}

data Weekday
  = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday
  deriving Show

{- Here is the 'Direction' type from Lecture 01 again: -}

data Direction
  = Up
  | Down
  | Left
  | Right
  deriving Show

{- In English, this says "define a datatype called 'Direction' whose
   values are either 'Up', 'Down', 'Left', or 'Right'. The 'deriving
   Show' part at the end tells Haskell to generate some code to print
   out these values. This will be covered in the section on Type
   Classes, below.

   As we have seen many times so far, we write functions that accept
   data as input using pattern matching. Here is a function that turns
   'Direction's into 'Transformation's (of 'Position's): -}

move :: Direction -> Transformation
move Up    (x,y) = (x,y+1)
move Down  (x,y) = (x,y-1)
move Left  (x,y) = (x-1,y)
move Right (x,y) = (x+1,y)

{- Unlike Java's enum types, datatypes in Haskell can also have
   additional data attached to them. We've seen several examples so
   far, including 'Cursor' and 'Process' in Exercise 1. For example,
   we could represent grid directions with attached distances like so: -}

data GridDirection
  = Vertical Int
  | Horizontal Int
  deriving Show

{- Meaning that we have two constructors of values in 'GridDirection',
   each of which takes an 'Int' as an additional argument. To show how
   these are used, we define functions by pattern matching that turn
   'Direction's into 'GridDirection's, and 'GridDirection's into
   'Transformation's: -}

toGridD :: Direction -> GridDirection
toGridD Up    = Vertical 1
toGridD Down  = Vertical (-1)
toGridD Left  = Horizontal (-1)
toGridD Right = Horizontal 1

moveOnGrid :: GridDirection -> Transformation
moveOnGrid (Vertical offset)   (x,y) = (x, y+offset)
moveOnGrid (Horizontal offset) (x,y) = (x+offset, y)

{- Just as for type synonyms, we can define datatypes with
   parameters. An exceedingly useful example of this is the 'Maybe'
   type, which is defined like so: -}

data Maybe a
  = Nothing
  | Just a
  deriving Show

{- In English: "define a new datatype 'Maybe' for every type 'a', with
   two values: 'Nothing', which takes no parameters, and 'Just', which
   takes a parameter of type 'a'.

   'Maybe' is useful for situations where data may be missing. It is
   often used where 'null' would be a suitable value in other
   languages, except that the possibility of "no value" is recorded in
   the type. For example, if we write a function that searches for a
   particular key in a list of key-value pairs, we can use 'Nothing'
   to represent the case when the value is not found: -}

search :: Eq k => k -> [(k,v)] -> Maybe v
search k []           = Nothing
search k ((k',v):kvs) = if k == k' then Just v else search k kvs

{- In words: searching for 'k' in the empty list returns 'Nothing';
   searching for 'k' in the list with (k',v) at the head returns 'Just
   v' if k is equal to k', and searches the rest of the list
   otherwise.

   While 'Maybe' is often used as a replacement for 'null', it is
   important to observe that it is marked explicitly in the type
   system. An analogue of this 'search' function in the Java standard
   library is the 'Map.get(Object)' method in the 'Map' interface:

      https://docs.oracle.com/javase/10/docs/api/java/util/Map.html#get(java.lang.Object)

   The documentation says that if the key searched for is not in the
   map, then this method returns null. However, it is valid for a key
   to be associated with the value 'null' in some implementations of
   'Map', so this is ambiguous.

   In Haskell, if the values could be 'null', then type of the list of
   key-value pairs would be of the form '[(k,Maybe v)]', and the
   'search' function would return values of type 'Maybe (Maybe
   v)'. Values of this type can be of one of three forms:

      1. Nothing       -- the key is missing
      2. Just Nothing  -- the key is present, but is mapped to Nothing
      3. Just (Just v) -- the key is present, and is mappped to 'v'

   In general, the presence of 'null' in programming language designs
   is now considered something of an antipattern. The inventor of
   'null', Sir Tony Hoare (also the inventor of QuickSort) now
   describes it as his "Billion dollar mistake", due to the number of
   bugs it has caused:

       https://www.infoq.com/presentations/Null-References-The-Billion-Dollar-Mistake-Tony-Hoare/

   Java now contains a class called 'Optional<V>' that is a clone of
   Haskell's 'Maybe' type. However, it is basically impossible to get
   rid of 'null' from Java completely.

   The lesson of 'Maybe' is that it is worth putting some effort into
   designing our types correctly to avoid illegal states from being
   representable, and to avoid confusion between different states. As
   an example, let's say that we want to represent student records
   with the following data:

     1. We always have student name

     2. We have either one or both of their registration number or DS
        username.

   If we try to do this in Java, we usually have to resort to comments
   to describe what is allowed:

        public class Student {
           // never null!
           @Nonnull
           private String name;

           // at least one of these is non-null
           private String registrationNumber;

           private String dsUsername;

           // ...
        }

   There is a '@Nonnull' annotation we can use to indicate that a
   particular reference field must be non null. However, the
   constraint that we have at least one of the registrationNumber and
   dsUsername is more difficult.

   In Haskell, we can make the following definitions: -}

data Student = MkStudent { name    :: String
                         , details :: StudentDetails
                         } deriving Show

data StudentDetails
  = RegNumber String
  | DSUsername String
  | RegNumAndDS String String
  deriving Show

{- This defines two datatypes. The first, 'Student', has a single
   constructor that takes two arguments. I have used another feature
   of Haskell, the ability to name the arguments to constructors. It
   is mainly here for documentation. Already in this type, we have
   encoded the fact that we *must* have a name for each student by
   *not* saying that we have a 'Maybe String'. We can also capture the
   constraint that we have at least one of the registration number or
   DS username by writing down the three cases explicitly in the
   'StudentDetails' type.

   The payoff for this is that it is not possible to make values of
   type 'Student' that do not satisfy the two constraints we listed
   above. By using proper type definitions, we have been able to take
   our comments and turn them into actionable advice for the machine
   to check our programs. -}


{- A special case of 'data' is when we have exactly one constructor
   which has exactly one argument. For example: -}

data Kilograms = MkKilograms Double
  deriving Show

{- This kind of declaration is very similar to the 'type Kilograms =
   ...' declaration from Part I, except that because 'data' always
   generates a *new* type, 'Kilograms' is different to 'Double'. This
   stops us from mixing up types as was permitted by just making type
   synonyms, at the cost of having to use pattern matching to
   decompose values.

   However, using 'data' for this purpose has a runtime cost. Due to
   the way that 'data' constructors in Haskell are considered to be
   lazy (we will cover this in Lecture 17), there is an overhead in
   the runtime representation of 'Kilograms'. To avoid this, Haskell
   offers another way of defining new types that are copies of
   existing types: 'newtype' -}

newtype Feet = MkFeet Double
  deriving Show

{- At runtime, the representation of 'Feet' is *exactly* the same as the
   representation of 'Double'. However, at compile time, the compiler
   treats these two types as distinct, and we have to use pattern
   matching and constructors to move between them.

   ASIDE: To see what the difference between 'data' and 'newtype' is
   in terms of laziness, have a look at this page from the Haskell
   wiki:

       https://wiki.haskell.org/Newtype

   or wait until Week 10. -}


{-    Part 5.2 : TYPE CLASSES

   In most of the datatype declarations I have written so far, there
   have been mysterious "deriving Show" bits written underneath. On
   some of the function definitions, there have been obscure bits like
   'Eq a =>' and 'Ord a =>'. Both of these relate to a feature of
   Haskell called "type classes".

   Type classes are "classes" of types that all satisfy some common
   interface. For instance, the 'Eq' type class is the class of all
   types that have a function '==' that computes whether two values of
   that type are equal. Similarly, the 'Ord' type class is the class
   of all types that (a) are in the 'Eq' type classes, and (b) also
   have functions '<', '<=', '>=', '>', and 'compare' for making
   ordering comparisons. The 'Show' type class is the class of all
   types that have a 'show' function for converting them to strings.

   Type classes are similar to "interfaces" in Java, and have very
   little to do with "classes" in Java. Just as we might say that a
   class "MyClass" in Java might implement the "Comparable" interface,
   we might say that a type 'MyType' in Haskell implement (or "has an
   instance for") the 'Ord' type class.

   To write a function that uses functions from a type class for an
   unknown type, we must add a constraint to the type signature of the
   function to state that we require that this type is a member of
   that class. We saw an example with 'search' above:

      search :: Eq k => k -> [(k,v)] -> Maybe k

   This type says that we can use 'search' with any type of keys 'k',
   as long as that type is a member of the 'Eq' type class. As we will
   see below, 'Int' is a member of the 'Eq' type class, so we can use
   'Int's as keys:

       *Week05> search 4 [(1,"one"),(4,"four")]
       Just "four"

   However, it isn't possible in general to compare functions for
   equality, so we can't use functions as keys. Trying to do so yields
   an error:

       *Week05> search (\x -> x) [(\x -> x, "identity"), (not, "negation")]
       <interactive>:...: error:
        • No instance for (Eq (Bool -> Bool))
            arising from a use of ‘search’
            (maybe you haven't applied a function to enough arguments?)
        • In the expression:
            search (\ x -> x) [(\ x -> x, "identity"), (not, "negation")]
          In an equation for ‘it’:
              it = search (\ x -> x) [(\ x -> x, "identity"), (not, "negation")]

   The first bullet point tells us the problem: there is no instance
   (i.e., "implementation") of 'Eq' for the type 'Bool -> Bool'.

   It is also worth looking at what happens if we try to define
   'search' without including the 'Eq' constraint. Doing so yields the
   following error:

       Week05.hs:...: error: …
           • No instance for (Eq k) arising from a use of ‘==’
             Possible fix:
               add (Eq k) to the context of
                 the type signature for:
                   search :: forall k v. k -> [(k, v)] -> Maybe v
           • In the expression: k == k'
             In the expression: if k == k' then Just v else search k kvs
             In an equation for ‘search’:
                 search k ((k', v) : kvs) = if k == k' then Just v else search k kvs

   Again, the relevant information is in the first bullet point. We've
   tried to use '==' on values of type 'k' without specifying that 'k'
   is a member of the 'Eq' type class.

   GHCi can give us information about currently defined type classes
   by using the ":info" feature:

       *Week05> :info Eq
       class Eq a where
         (==) :: a -> a -> Bool
         (/=) :: a -> a -> Bool
         {-# MINIMAL (==) | (/=) #-}
               -- Defined in ‘GHC.Classes’
       instance (Eq a, Eq b) => Eq (Either a b)
         -- Defined in ‘Data.Either’
       instance Eq a => Eq [a] -- Defined in ‘GHC.Classes’
       instance Eq Word -- Defined in ‘GHC.Classes’
       instance Eq Ordering -- Defined in ‘GHC.Classes’
       instance Eq Int -- Defined in ‘GHC.Classes’
       instance Eq Float -- Defined in ‘GHC.Classes’
       instance Eq Double -- Defined in ‘GHC.Classes’
         ....

   Asking for information on the 'Eq' type class first gives us its
   definition:

       class Eq a where
         (==) :: a -> a -> Bool
         (/=) :: a -> a -> Bool
         {-# MINIMAL (==) | (/=) #-}

   telling us that types in the 'Eq' type class actually have two
   'methods': (==) for equality, and (/=) for disequality. The comment
   'MINIMAL' tells us that we only need to define one of these, since
   the other can be defined in terms of it (by negation, in this
   case).

   The rest of the output tells us about all the types in the 'Eq'
   type class that GHCi currently knows about, including 'Int' as we
   used above. Defining new types with 'deriving Eq', defining our own
   instances of 'Eq', and importing other modules will extend this
   list.

   Haskell comes with definitions of 'Eq a' for many of the built in
   types. But what if we define our own types, like we did above? We
   have two options.

   First, we can get Haskell to define an equality test for us. This
   is what the 'deriving Eq' line does. For example: -}

data CompassDirection
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

{- This 'data' declaration defines a four element type of compass
   directions, and asks the Haskell compiler to generate
   implementations of the 'Eq' and 'Show' type classes for us. This
   now allows us to compare 'CompassDirection's for equality:

      λ> North == North
      True
      λ> North == South
      False

   And to use them in functions that require a member of the 'Eq' type
   class, such as 'search' defined above:

      λ> search North [(North,Up),(South,Down)]
      Just Up
      λ> search West [(North,Up),(South,Down)]
      Nothing

   Deriving an implemenation of the 'Show' type class means that we
   get a function called 'show' that converts values of
   'CompassDirection' to 'String's. We can get information on 'Show'
   by using ':info' again:

      class Show a where
        showsPrec :: Int -> a -> ShowS
        show :: a -> String
        showList :: [a] -> ShowS
        {-# MINIMAL showsPrec | show #-}

   The useful function is called 'show'. The 'showsPrec' function is
   useful for outputting data without too many parentheses (using
   'prec'edence information), and 'showList' is sometimes a useful
   optimisation for outputting many values.

   Since we have derived a Show implementation for 'CompassDirection',
   we can use "show" on its values:

       λ> show North
       "North"
       λ> show South
       "South"

   The 'show' function is analogous to the 'toString' method on the
   'Object' class in Java.

   The ability of the Haskell compiler to generate instances of type
   classes for us is very useful, as long as we agree with the choices
   it makes. As an example of where we might want to differ from the
   default choice, let's look at the example of making a type of case
   insensitive strings. The only way that case insensitive strings
   differ from normal strings is that we regard two case insensitive
   strings as equal if they only differ in the cases of the characters
   in them. We make a 'newtype' of 'CaseInsenstiveString's, deriving
   'Show', but *not* 'Eq': -}

newtype CaseInsenstiveString = CIS String
  deriving Show -- but not 'Eq'!

{- We now use an 'instance' declaration to define what it means for two
   strings to be equal up to caseinsensitivity. We implement this by
   mapping all characters in both strings to upper case and then
   comparing them using normal string equality: -}

instance Eq CaseInsenstiveString where
  CIS str1 == CIS str2 =
    map toUpper str1 == map toUpper str2

{- In general, instance declarations define implementations for all the
   'methods' defined in the type class.

   Now we have:

       λ> CIS "case" == CIS "CASE"
       True
       λ> CIS "case" == CIS "CaSe"
       True
       λ> CIS "case" == CIS "C4S3"
       False

   Defining our own 'Eq' instances is often useful when we want to
   make more things equal than are usually so, as in this
   example. -}


{-    Part 5.3 : SEMIGROUPS AND MONOIDS

   It is also possible to define our own type classes. However, it is
   not very common to do this in normal Haskell programming. The
   standard library defines a large range of useful ones, and defining
   new type classes that are generally useful is actually quite
   difficult. Simply enabling name overloading is not a good enough
   reason to define a new type class. A new type class should ideally
   capture some coherent concept the makes sense in across a range of
   application domains.

   As an example of the syntax for declaring type classes, I'll show
   two type classes from the standard library that will be very
   useful: 'Semigroup's and 'Monoid's.

   A semigroup is a type 'a' that has an operation:

     (<>) :: a -> a -> a

   which we read as some kind of "plus" or "combination" operation. We
   also expect this operation to be associative, meaning that it
   satisfies this equation:

     (x <> y) <> z == x <> (y <> z)

   A monoid is a semigroup that also has a 'zero' value, called
   'mempty', which satisfies the following laws:

     mempty <> x == x
     x <> mempty == x

   The simplest example of a monoid (or, more properly, a type with
   monoid structure), is the type 'Integer', with addition (+) as the
   (<>) and 0 as 'mempty'.

   Monoids are useful as a common interface to the ability to
   aggregate a number of data values into one. The (<>) operation
   combines two values, and the 'mempty' tells us what to do if we
   have no values. Below, we'll see how to use Monoids for the purpose
   of data aggregation. For now, we'll see how to define the concepts
   of Semigroup and Monoid in Haskell, and how to define instances of
   those concepts.

   First we define the 'Semigroup' type class: (this type is already
   defined in the standard library, but I hid the definition in the
   import line at the top of this module). -}

class Semigroup a where
  (<>) :: a -> a -> a

{- This declaration says that there is a class of types called
   'Semigroup', and every member 'a' of this class has an operation
   called (<>) which has type 'a -> a -> a'.

   We can now define some instances of this class: -}

instance Semigroup Int where
  (<>) = (+)

instance Semigroup Bool where
  (<>) = (&&)

instance Semigroup [a] where
  (<>) = (++)

data Fun a = MkFun (a -> a)

instance Semigroup (Fun a) where
  MkFun f <> MkFun g = MkFun (f . g)

{- With the Semigroup type class, we get to re-use the same symbol "<>"
   to mean "combine" instead of the using the specific one:

     *Week05> 1 <> 2
     3.0

     *Week05> True <> False
     False

     *Week05> [1,2,3] <> [4,5,6]
     [1,2,3,4,5,6]

   In the first case, Haskell has worked out that we mean 'Double's,
   in the second we mean 'Bool's and in the third we means lists, and
   has executed the correct code in each case. It is using the type
   information to select the right implementation to take.

   The ability to use the same symbol to mean "combine" for lots of
   different things is not obviously useful on these small
   examples. In each case, it would be clearer to use the specific
   operation.

   Where the generic operation "<>" is useful is in writing generic
   functions that combine multiple things from some data
   structure. Here is a small example. If we define a type of
   'Triple's, where all three elements are of the same type: -}

data Triple a = MkTriple a a a
  deriving Show

{- Now we may wish to "sum up" a Triple. Instead of writing specific
   functions for when we have 'Triple Int', 'Triple Double' and so on,
   we can write a single function that combines Triples of any type of
   values, as long as that type is a Semigroup: -}

combineTriple :: Semigroup a => Triple a -> a
combineTriple (MkTriple x y z) = x <> y <> z

{- Now we can combine triples of numbers, booleans, and lists, all with
   one function:

     *Week05> combineTriple (MkTriple 1 2 3)
     6.0

     *Week05> combineTriple (MkTriple True True True)
     True

     *Week05> combineTriple (MkTriple [1,2,3] [4,5,6] [7,8,9])
     [1,2,3,4,5,6,7,8,9]

   A more useful function is the one that combines a whole list of
   elements into one, aggregrating them in some way. To actually
   define this, we are going to need to extend the Semigroup type
   class with the concept of a "zero". For historical reasons, a
   Semigroup with a zero is called a monoid: -}

class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a
  mappend = (<>)

{- This says that a type 'a' is a Monoid if (1) it is already a
   'Semigroup', (2) it implements 'mempty', and (3) it implements
   'mappend' with the default implementation (<>).

   (The existence of the two names '<>' and 'mappend' for the same
   thing is a historical accident of the Haskell library, and nothing
   to do with the theory of semigroups or monoids.)

   Now we can extend our two Semigroup instances to be Monoids as
   well: -}

instance Monoid Int where
  mempty = 0

instance Monoid Bool where
  mempty = True

instance Monoid [a] where
  mempty = []

instance Monoid (Fun a) where
  mempty = MkFun (\x -> x)

{- ASIDE: That the equations for semigroups and monoids we stated above
      do actually hold for these definitions, but unfortunately
      Haskell has no way of checking this. The CS410 course next year
      will introduce the Agda system, which includes a proof language
      that allows for properties like this to be proved. The Liquid
      Haskell system is another system which allows proving properties
      of Haskell programs by putting in special comments:

         https://ucsd-progsys.github.io/liquidhaskell-blog/
-}

{- Now that we have 'Monoid' defined, we can write a function that "sums
   up" a list for any Monoid instance: -}

combineList :: Monoid a => [a] -> a
combineList []     = mempty
combineList (x:xs) = x <> combineList xs

{- With this function, we have a single implementation that works for
   many different kinds of "summing up" of lists.

   Actual summing up: -}

sum' :: [Int] -> Int
sum' = combineList

{- Concatenating a list of lists: -}

concat' :: [[a]] -> [a]
concat' = combineList

{- Determining whether a list of booleans are all 'True': -}

allTrue :: [Bool] -> Bool
allTrue = combineList

{- This last one uses the fact that we defined the combination of two
   'Bool's to be their AND. If we also use a 'map' to preprocess the
   list, we can get a function that checks whether or not a predicate
   is true for all elements of a list: -}

forall :: (a -> Bool) -> [a] -> Bool
forall p = combineList . map p


{-    Part 5.4 : FOLDABLE TYPE CLASS

   Lists are not the only kind of container for which we can "add up"
   all the elements. We can also do this for 'Tree's, using 'mempty'
   for the 'Leaf's, and 'mempty' twice to add up the results of the
   left tree, the data, and the right subtree. Note that the order in
   which we do this:

         (left <> x) <> right,
      or
         left <> (x <> right)

   doesn't matter by the associativity law for "Semigroup"s. -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving (Show)

combineTree :: Monoid m => Tree m -> m
combineTree Leaf         = mempty
combineTree (Node l x r) = combineTree l <> x <> combineTree r

{- We can also "add up" all the elements stored in a "Maybe"
   container. Since "Maybe"s can contain at most one thing, we don't
   need to use 'mappend', but we use 'mempty' for the "Nothing" case. -}

combineMaybe :: Monoid m => Maybe m -> m
combineMaybe Nothing  = mempty
combineMaybe (Just x) = x

{- Now we've seen three examples of containers that we can 'fold'
   over. As we did for 'Functor', let's compare their types:

      combineList  :: Monoid m => [m]     -> m
      combineTree  :: Monoid m => Tree m  -> m
      combineMaybe :: Monoid m => Maybe m -> m

   As before, these only differ in the container type. This suggests
   that it might be worth declaring a new typeclass for "Foldable"
   containers. We get the type by generalising from the specific
   containers to a generic 'c': -}

class Foldable c where
  fold :: Monoid m => c m -> m

{- Declaring instances for our collection of data types: -}

instance Foldable [] where
  fold = combineList

instance Foldable Tree where
  fold = combineTree

instance Foldable Maybe where
  fold = combineMaybe

{- We can now write a generic function for summing up any container full
   of integers, by specialising 'fold' to the case when the 'Monoid'
   'm' is 'Int'. This subsumes the 'sumList' function we wrote above,
   and also gives us a way to sum up 'Tree's and 'Maybe's and any
   other containers we may define in the future. -}

sumGeneric :: Foldable c => c Int -> Int
sumGeneric = fold

{- And a generic function for any Foldable container full of Booleans: -}

andGeneric :: Foldable c => c Bool -> Bool
andGeneric = fold

{- But what if the container we have is not already full of 'Bool's or
   'Int's? What if it is full of things that can be transformed to
   'Bool's and 'Int's like in the 'forall' example above? In that
   example, we want to take:

    1. A container full of 'a's.
    2. A predicate from 'a's to 'Bool's

   And work out if every 'a' makes the predicate true. To do this, we
   need to turn a container full of 'a's into a container full of
   'Bool's. This is exactly the job of the 'Functor' typeclass, as we
   will see in the next part. -}



{-    Part 5.5 : FUNCTOR TYPE CLASS

   In Week 05 (Higher Order Functions), we saw the function 'map',
   which applies a function to every element of a list to yield a new
   list of tranformed elements:

      map :: (a -> b) -> [a] -> [b]

   We also saw a map operation for 'Tree's, which has this type:

      mapTree :: (a -> b) -> Tree a -> Tree b

   These functions both a do similar thing: they take a function 'f',
   some structure containing values of type 'a', and return the *same*
   structure, but this time containing values of type 'b'. We can draw
   this graphically. The function 'map' works on lists:

          [ a1, a2, ..., an ]
            |   |        |
            v   v        v
          [ b1, b2, ..., bn ]

   where b1 = f a1, b2 = f a2, ..., bn = f an.

   Similarly, for trees, we have, for example:

      Node (Node Leaf a1 Leaf) a2 (Node Leaf a3 Leaf)
                      |        |             |
                      v        v             v
      Node (Node Leaf b1 Leaf) b2 (Node Leaf b3 Leaf)

   where, again, b1 == f a1, and so on.

   The important point to see here is that in both cases, mapping does
   not affect the *structure* of the container, only the values stored
   within it. This is an important enough concept that there is a
   special name for it.

   Type constructors that support an operation analogous to 'map' are
   called "Functors". Using the type class feature of Haskell, we can
   capture this concept within the language rather than just making it
   a convention.

   To construct the Functor type class, let's look at the type of the
   four mapping functions we saw in Week 03 together so we can see how
   to generalise them:

       map           :: (a -> b) -> [a]          -> [b]
       mapTree       :: (a -> b) -> Tree a       -> Tree b
       mapMaybe      :: (a -> b) -> Maybe a      -> Maybe b
       mapWithString :: (a -> b) -> WithString a -> WithString b

   The only place where these differ is the name of the container
   type. For 'map' it is '[]', meaning "lists". For 'mapTree' it is
   'Tree'. Similar in spirit to how we generalised from the specific
   to the general in Week 03 (Higher-Order functions), we replace the
   specific '[]' or 'Tree' with a generic 'c' to get:

       fmap   : (a -> b) -> c a -> c b

   We read this as "given a way of transforming 'a's to 'b's, we get a
   way of transforming a 'c' container full of 'a's into a 'c'
   container full of 'b's. If we replace 'c' with '[]' or 'Tree', we
   get the types of 'map' and 'mapTree' above.

   Due to the diversity of different sorts of containers, it isn't
   going to be possible to write one 'fmap' function with this
   type. Instead, we must write a separate one for each kind of
   container. Similar to how each type can have its own 'show'
   function, with a common interface described by the 'Show' typeclass
   above, we define a typeclass 'Functor' that describes this common
   interface. (The name 'Functor' is chosen for historical reasons,
   one might also call it 'Mappable'.) -}

class Functor c where
  fmap :: (a -> b) -> c a -> c b

{- As we did for 'Show', we now write "instances" of the 'Functor' type
   class for each of the container types we've seen. For lists, the
   built-in 'map' function does what we want: -}

instance Functor [] where
  fmap = map

{- For 'Tree's, we need to implement 'fmap' ourselves, since it is a
   type we defined ourselves. In Exercise 3.1.2, we ask you to
   implement 'mapTree' using 'iterTree'. Here, we define it directly
   using pattern matching: -}

instance Functor Tree where
  fmap f Leaf         = Leaf
  fmap f (Node l x r) = Node (fmap f l) (f x) (fmap f r)

{- 'Maybe' is also an instance of 'Functor'. Following our intuition
   that 'fmap' should not change the shape of the data, only the data
   stored within, we map 'Nothing' to 'Nothing', and 'Just' to 'Just',
   using 'f' to transform the data in the latter case: -}

instance Functor Maybe where
  fmap f Nothing  = Nothing
  fmap f (Just a) = Just (f a)

{- It is worth taking a while to look at these definitions (and the one
   for 'map') to see how they are similar, despite the different kinds
   of container.

   In every case, the constructor that is being matched on the
   left-hand side ('[]', ':', 'Leaf', 'Node', 'Nothing', 'Just') also
   appears on the right-hand side. Also, whenever there is a
   substructure (i.e., the rest of the list, the two subtrees), we
   apply 'fmap' to those as well.

   Together, these two observations indicate that our inituition about
   functors above was correct -- a 'mapping' function for a container
   preserves shapes, but transforms stored data.

   As we saw in Week 03, not all datatypes have a 'map'-like
   function. One example was the 'Fun' datatype. To formally give a
   reason for excluding it, and to provide useful reasoning principles
   for Functors, we require that 'Functor' instances always obey two
   equational laws that intuitively state that 'fmap' does do
   modification of values and not structure.

   The laws are:

      1. fmap id c == c

         Mapping the identity function over a container should not
         affect the container or its values at all. This is reasonable
         -- if we do nothing to the values stored in the container,
         then the whole thing should be unaffected.

      2. fmap f (fmap g c) == fmap (f . g) c

         If we map a function 'g' over a container, and then map a
         function 'f' over the result, then that ought to be the same
         as just mapping their composition. Again, this is reasonable:
         if we are leaving the structure of a container untouched,
         then it shouldn't matter how many times we traverse over it
         to alter the values stored in it. -}

{- Combining 'Foldable' and 'Functor' gives us a way to preprocess
   elements before folding them up. For example, if we transform every
   element in a container to '1', and then add them up, we get a
   generic 'size' function: -}

size :: (Functor c, Foldable c) => c a -> Int
size = fold . fmap (\ _ -> 1)

{- Here, we have used the function composition operator '(.)' that we
   talked about in Lecture 6 (Higher-Order Functions).

   If we have a predicate on elements of the container, then we can
   write a generic function that checks to see whether all the
   elements of a container satisfy that predicate: -}

all :: (Functor c, Foldable c) => (a -> Bool) -> c a -> Bool
all p = fold . fmap p

{- The combination of 'fold' and 'fmap' is so common that the Haskell
   standard library's definition of 'Foldable' already includes
   'foldMap' as a function. This can be defined as: -}

foldMap :: (Functor c, Foldable c, Monoid m) => (a -> m) -> c a -> m
foldMap f = fold . fmap f

{- However, in some cases it might be more efficient to define a special
   'foldMap' function that does not generate an intermediate data
   structure in between the 'fmap f' and the 'fold'.

   One of the wrinkles of Haskell's typeclass feature is that we
   cannot define multiple instances of a typeclass for the same
   type. This would be desirable for 'Monoid' especially. Above, we
   define 'True' and '&&' as the 'mempty' and 'mappend' of the
   'Monoid' instance for 'Bool'. But this is not the only choice.

   If we want to write the 'any' function that "or"s together a list
   (dual to the "all" function that "and"s together a list), then we
   would like to write:

       instance Monoid Bool where
          mempty  = False
          mappend = (||)

   But GHC complains:

      Lec09.hs:317:10-20: Duplicate instance declarations: …
            instance Monoid Bool
               -- Defined at .../Lec09.hs:317:10
            instance Monoid Bool
               -- Defined at .../Lec09.hs:468:10

      Compilation failed.

   This makes sense: if we defined two instance of 'Monoid' for
   'Bool', then Haskell wouldn't know which one to use.

   The solution is to use Haskell's newtype feature to give 'Bool's a
   new name, which we can use to define an instance that uses 'False'
   and '||' instead of 'True' and '&&'.

   (A "newtype" is like a "data", but we can only have one constructor
    with one argument. At runtime, "newtype"s disappear completely and
    in memory this is stored simply as a normal value of 'Bool' type
    (unlike with "data", which is tagged with the constructor at
    runtime).)

   We define a new name for 'Bool', called 'Any'. The "newtype"
   declaration names the constructor we will use for converting
   'Bool's to 'Any's: -}

newtype Any = MkAny Bool

{- To convert back, we write an 'unAny' function: -}

unAny :: Any -> Bool
unAny (MkAny x) = x

{- (We could have written:

        newtype Any = MkAny { unAny :: Bool }

    as a compact way of defining 'Any', 'MkAny' and 'unAny' all in one
    go.)

   Now we can write a 'Monoid' instance for 'Any' that uses 'False'
   and '||'. This definition is a bit noisy due to the 'MkAny's
   everywhere, but it is essentially the same as the 'Monoid' instance
   for 'Bool' above that uses 'True' and '&&': -}

instance Semigroup Any where
  MkAny x <> MkAny y = MkAny (x || y)

instance Monoid Any where
  mempty = MkAny False

{- Equipped with this, we can now write a generic 'exists' function that
   returns 'True' if there is any element in a container that
   satisfies the predicate: -}

exists :: (Functor c, Foldable c) => (a -> Bool) -> c a -> Bool
exists p = unAny . fold . fmap (MkAny . p)


{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- 1. Define a 'Show' instance for the following datatype that prints
      out the data in a JSON-like format. For example,

         show (MkHost "www.cis.strath.ac.uk" 80) == "{\"name\":\"www.cis.strath.ac.uk\", \"port\": 80}"
:-}

data Host = MkHost String Int

-- answer:
instance Show Host where
   show (MkHost name port) = "{\"name\":\"" ++ name ++ "\", \"port\": " ++ show port ++ "}"


{- 2. Define an 'Eq' instance for the following datatype that makes two
      numbers equal if they have the same remainder after division by
      12 (use the 'mod' function to get remainders: '14 `mod` 12 ==
      2). -}

data ClockHour = MkClockHour Int

-- answer:
instance Eq ClockHour where
   MkClockHour hour1 == MkClockHour hour2 = (hour1 `mod` 12) == (hour2 `mod` 12)

{- 3. Define Semigroup and Monoid instances for the following datatype
      that take the 'max' of the first element and 'min' of the
      second, and where 'mempty' is the pair with '0', and '1'. -}

data ForAndAgainst = Evidence Double Double

instance Monoid ForAndAgainst where
   mempty = Evidence 0 1

instance Semigroup ForAndAgainst where
   Evidence for1 against1 <> Evidence for2 against2 =
      Evidence (max for1 for2) (min against1 against2)

{-    (Such a datatype might be used for aggregating degrees of evidence
       for and against something. The first 'Double' is the strength
       of evidence for, and the second 'Double' is the strength of
       evidence against.) -}

{- 4. Define a Foldable instance for the following datatype: -}

data OneTwoOrThree a
  = One a
  | Two a a
  | Three a a a
  deriving Show

-- The foldable instance for this type has to "add up" one, two, or
-- three things depending on which constructor is used:

instance Foldable OneTwoOrThree where
  fold (One x)       = x
  fold (Two x y)     = x <> y
  fold (Three x y z) = x <> y <> z

-- The order that we add things up is up to us to pick (remember that
-- it is not always the case that 'x <> y == y <> x'), but
-- left-to-right seems like a good default.

{- 5. If you have a

          foldMap :: Monoid b => (a -> b) -> C a -> b

      for some container type C, can you always define a 'fold'? -}

-- Yes, because we can put in the 'id' (identity) function (also
-- written as '\x -> x'), to do nothing to the elements of the
-- container before folding them down to a single value:

-- fold = foldMap id


{- 6. Define a function of the type:

        toList :: (Functor c, Foldable c) => c a -> [a]

      which shows that with 'Foldable' you can always define a
      'toList' function. If you only have a 'toList' function for a
      container can you always define 'fold'? -}

-- To do this, we take the following strategy. Our input is a
-- container full of 'a's. For example, a list full of 'Int's:

---   [ 1,   2,   3,   4  ]

-- For each value in the container, we transform it to a list
-- containing only that value:

---   [[1], [2], [3], [4] ]

-- Then we do a 'fold' to concatenate all these lists to get a final
-- result list:

---   [ 1,   2,   3,   4  ]

-- This seems a like a silly way to convert lists to themselves, but
-- once we have it working for any container that is a Functor and a
-- Foldable, then the same function will work. For example, the same
-- idea works for Trees:

---    Node (Node Leaf  4  Leaf)  5  (Node Leaf  7  Leaf)
---    Node (Node Leaf [4] Leaf) [5] (Node Leaf [7] Leaf)
--     [                4   ,     5     ,        7   ]

-- To accomplish the full solution, we start with the container 'c',
-- run fmap with the anonymous function '\x -> [x]' to convert every
-- element to a one-element list, and then use 'fold' to concatenate
-- all the lists:

toList :: (Functor c, Foldable c) => c a -> [a]
toList c = fold (fmap (\x -> [x]) c)

{- 7. Define a Functor instance for the OneTwoOrThree type above: -}

-- The Functor instance for 'OneTwoOrThree' follows the general idea
-- of all 'map'-like functions: we preserve the shape (the
-- constructors used), but alter the stored values using the function
-- 'f'.

instance Functor OneTwoOrThree where
   fmap f (One x)       = One (f x)
   fmap f (Two x y)     = Two (f x) (f y)
   fmap f (Three x y z) = Three (f x) (f y) (f z)


{- 8. Use 'newtype' to define a generic 'product' function that computes
      the product (i.e. multiplies) all the numbers in a container: -}

-- We follow the same strategy as the 'Any' type above to take the
-- 'Or' of a container full of Bools, and use a newtype:

newtype MulInt = MkMulInt Int
  deriving Show

instance Semigroup MulInt where
   MkMulInt x <> MkMulInt y = MkMulInt (x * y)

instance Monoid MulInt where
   mempty = MkMulInt 1

unMulInt :: MulInt -> Int
unMulInt (MkMulInt x) = x

productGeneric :: (Functor c, Foldable c) => c Int -> Int
productGeneric c = (\(MkMulInt x) -> x) (fold (fmap MkMulInt c))
