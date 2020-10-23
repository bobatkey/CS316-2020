{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module Ex2 where

import Prelude                 hiding (foldr)
import Control.Exception       (finally)
import Data.ByteString.Builder (Builder, word32LE, word8, word16LE, hPutBuilder)
import Data.Foldable           (fold)
import Data.Semigroup          (Semigroup ((<>)))
import Data.Word               (Word8, Word32)
import System.IO               (openFile, IOMode (..), hClose)

{----------------------------------------------------------------------}
{- CS316 (2020/21) EXERCISE 2 : HIGHER-ORDER PROGRAMMING              -}
{----------------------------------------------------------------------}

{- Please read this file carefully. Questions are numbered 2.X.Y and
   usually consist of unfinished definitions that you will have to
   fill in. The marks available for each question are listed
   underneath the question and answer space. There are four
   independent parts, with 31 questions overall.

   Submission instructions:

    - Deadline: 23:00 Thursday 5th November
    - Location: CS316 MyPlace

   This exercise is worth 30% of your overall mark for CS316. It is
   marked out of 60, so one mark here is worth half a percentage
   point.

   Note about plagiarism: For the take home parts of this exercise,
   you can discuss the questions with others to make sure that you
   understand the questions. However, you must write up your answers
   by yourself. Do not share your solutions online (e.g., on GitHub or
   other code sharing platforms. Use a private repository if you want
   have a backup).

   Plagiarism will be taken very seriously. -}

{----------------------------------------------------------------------}
{- HIGHER ORDER PROGRAMMING                                           -}
{----------------------------------------------------------------------}

{- This exercise is focused on programming with "Higher order"
   functions, as introduced in Week 03.

   A higher order function is a function that takes other functions as
   input. The name 'higher order' comes from the following
   classification of entities that one might find in a programming
   language:

   - 0th order entities are "data" (basically anything that can be
     printed out)
   - 1st order entities are functions that take "data" to "data"
   - 2nd order entities are functions that take (functions that take
     "data" to "data") to "data"
   - 3rd order entities are functions that take 2nd order entities to
     "data"
   - .. and so on

   It is rare to see anything above 3rd order, but it can occur.

   Programming with higher order functions is sometimes called
   programming with "first class" functions. This references the idea
   that functions in Haskell are values, just like any other
   value. They can be stored in data structures, returned by
   functions, and passed as arguments to functions. That is, they are
   'first class' elements of the language, instead of being "second
   class" elements with a restricted set of operations.

   In other languages, functions that take other functions as input
   are often said to take a 'callback' function. Examples include
   methods that perform work asynchronously and call the given
   function when the work is done. Another example is the
   'java.util.List.sort' method:

      https://docs.oracle.com/javase/9/docs/api/java/util/List.html#sort-java.util.Comparator-

   which takes a 'Comparator' argument, which is (in Java terminology)
   a 'Functional Interface', which is essentially a function. We'll
   see an example of a sorting function that takes a compare as an
   argument later on. -}

{----------------------------------------------------------------------}
{- PART 1 : HIGHER ORDER FUNCTIONS ON LISTS AND TREES                 -}
{----------------------------------------------------------------------}

{- 2.1.0 Discarding.

   Write filter's evil twin that retains the elements of a list that
   fail the test rather than those that pass.

   Write your function using 'filter'. Do *not* write it as a
   recursive function. -}

discard :: (a -> Bool) -> [a] -> [a]
discard = undefined

{- 1 MARK -}


{- 2.1.1 Summing lengths

   Use 'sum', 'map', and 'length' to compute the sum of the lengths of
   all the lists in a list of lists. For example:

      sumLengths [[], [], []]    == 0
      sumLengths [[1,2], [3,4]]  == 4
      sumLengths [[1], [2], [3]] == 3
      sumLengths [["no", "matter"],["what"],["is","in","the"],["lists"]] == 7
-}

sumLengths :: [[a]] -> Int
sumLengths = undefined

{- 2 MARKS -}


{- 2.1.2 Filtering and Counting

   Use 'length', 'filter' and an anonymous function to compute the
   number of elements in a list of pairs of strings and integers where
   the integer part is greater than 10. -}

numGreaterThanTen :: [(String,Int)] -> Int
numGreaterThanTen = undefined

{- 2 MARKS -}


{- 2.1.3 One-pass Average.

   Here is the 'foldr' function from Week 04. -}

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f b []     = b
foldr f b (x:xs) = f x (foldr f b xs)

{- As presented in the lecture, it is possible to use 'foldr' to
   implement many other interesting functions on lists. For example
   'sum' and 'len': -}

sumDoubles :: [Double] -> Double
sumDoubles = foldr (\x sum -> x + sum) 0

len :: [a] -> Integer
len = foldr (\_ l -> l + 1) 0

{- Putting these together, we can implement 'avg' to compute the average
   (mean) of a list of numbers: -}

avg :: [Double] -> Double
avg xs = sumDoubles xs / fromInteger (len xs)

{- Neat as this function is, it is not as efficient as it could be. It
   traverses the input list twice: once to compute the sum, and then
   again to compute the length. It would be better if we had a single
   pass that computed the sum and length simultaneously and returned a
   pair.

   Implement such a function, using foldr: -}

sumAndLen :: [Double] -> (Double, Integer)
sumAndLen = undefined

{- Once you have implemented your 'sumAndLen' function, this alternative
   average function will work: -}

avg' :: [Double] -> Double
avg' xs = total / fromInteger length
  where (total, length) = sumAndLen xs

{- 2 MARKS -}


{- 2.1.4 mapTree from foldTree

   Here is the 'Tree' datatype from the previous Exercise: -}

data Tree a
  = Leaf
  | Node (Tree a) a (Tree a)
  deriving Show

{- As we saw in Week 04, it is possible to write a generic recursor
   pattern for trees, similar to 'foldr': -}

foldTree :: b -> (b -> a -> b -> b) -> Tree a -> b
foldTree l n Leaf           = l
foldTree l n (Node lt x rt) = n (foldTree l n lt) x (foldTree l n rt)

{- Implement 'mapTree' in terms of 'foldTree': -}

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree = undefined

{- Here is the explicitly recursive version of 'mapTree', for
   reference: -}

mapTree0 :: (a -> b) -> Tree a -> Tree b
mapTree0 f Leaf           = Leaf
mapTree0 f (Node lt x rt) = Node (mapTree0 f lt) (f x) (mapTree0 f rt)


{- 2 MARKS -}


{- 2.1.5 Flattening trees

   Use 'foldTree' to flatten a tree to list in left-to-right order: -}

flatten :: Tree a -> [a]
flatten = undefined

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
   class in Lecture 04 to write sorting functions. If a type is a
   member of the Ord type class we can compare values of that
   type. This is what allows us to use '<' and '>='. The Ord type
   class also defines a function 'compare':

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
   order. Below, we will write sorting functions that take the
   ordering to use as an explicit argument.

   To do this, we need to isolate the idea of a thing that compares
   two values of the same type. We will use first class functions to
   do this.

   A 'Comparator' in Haskell is a function that takes two values and
   returns an 'Ordering' (satisfying some properties). Let's make a
   type synonym for Comparators: -}

type Comparator a = a -> a -> Ordering

{- Every type that is a member of the 'Ord' type class has a default
   comparator. We just write 'compare' for this, and Haskell's type
   inference mechanism will work out which one to use. However, the
   default comparator might not be the ordering that we wish to
   use. We'll now see how to build new comparators out of old ones.

   (To be a proper comparator, we ought to also have some properties
    for any comparator 'cmp':

      1. cmp x y == invertOrdering (cmp y x)
      2. if (cmp x y == LT) and (cmp y z == LT) then (cmp x z == LT)
      3. if (cmp x y == EQ) then, for all z, (cmp x z == cmp y z)

    We won't get into worrying about these for this exercise though.) -}

{- 2.2.0 Inverting Comparators.

   We can invert an 'Ordering': -}

invertOrdering :: Ordering -> Ordering
invertOrdering LT = GT
invertOrdering EQ = EQ
invertOrdering GT = LT

{- Write a function that takes as input a comparator and returns a
   comparator that implements the reverse ordering. Use
   'invertOrdering'. -}

invert :: Comparator a -> Comparator a
invert cmp x y = undefined

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

   Here is a Haskell implementation of merge sort that is similar to
   the one we saw in the Week 02 tutorial questions. 'mergeSort'
   handles the main recursion described above, 'split' does the
   splitting, and 'merge' does the merging.

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
mergeSortWith = undefined

mergeWith :: Comparator a -> [a] -> [a] -> [a]
mergeWith = undefined


{- Make sure you don't accidentally call 'mergeSort' in the recursive
   calls!

   It should be the case that 'mergeSortWith compare' always gives the
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


{- 2.2.3 Dictionary ordering

   Write a function that takes two comparators, one that compares 'a's
   and one hat compares 'b's and makes a comparator for pairs '(a,b)',
   such that for any two pairs (a1,b1), (a2, b2):

     - if a1 < a2 then (a1,b1) < (a2,b2)
     - if a1 > a2 then (a1,b1) > (a2,b2)
     - if a1 = a2 then (a1,b1) and (a2,b2) are ordered however b1, b2 are

   (this is called "lexicographic ordering", or "dictionary ordering")

   For example:

        > (pair compare compare) (1,2) (1,3)
        LT
        > (pair compare compare) (2,2) (1,3)
        GT
        > (pair (invert compare) compare) (1,2) (1,3)
        LT
        > (pair (invert compare) compare) (2,2) (1,3)
        LT

   Hint: use a 'case'. -}

pair :: Comparator a -> Comparator b -> Comparator (a,b)
pair = undefined

{- 3 MARKS -}


{- 2.2.4 Using 'mergeSortWith' and the functions above, write a function
   that sorts lists of '(Int,String)' in reverse order on the length
   of the second element of each pair.

   You should use the 'on' function, don't write the comparison by
   hand. You will also find the 'length', 'snd', and 'compare'
   functions useful. -}

sortOnReverseSndLength :: [(Int,String)] -> [(Int,String)]
sortOnReverseSndLength = undefined

{- Example:

     > sortOnReverseSndLength [(1,"one"), (2,"two"), (3,"three"), (4,"four"), (5, "five"), (6, "six")]
     [(3,"three"),(4,"four"),(5,"five"),(6,"six"),(2,"two"),(1,"one")]
-}

{- 2 MARKS -}


{----------------------------------------------------------------------}
{- PART 3 : PICTURES                                                  -}
{----------------------------------------------------------------------}

{- In this part of the exercise, you will build a small graphics
   library. Pictures will be represented as functions from 'Point's to
   values: -}

type Picture a = Point -> a

{- where a Point is an (x,y) coordinate: -}

type Point = (Double, Double)

{- 'Picture's are parameterised by the type of data that can appear at
   each coordinate. For example, to represent pictures where we only
   care whether a pixel is set or not we might use the type:

      Picture Bool

   we will use this type as a type of 'masks' for filtering other
   pictures. We could think of these as pictures that have two
   colours: True for black and False for white (or the other way round
   -- until we render them, these pictures are only in our heads). -}

{- For 'real' images, we want every point in the coordinate space to be
   associated with a colour. We will represent colours as their RGB
   components, with an Alpha channel for transparency. The type for
   representing colours with transparency is: -}

data RGBA = MkRGBA { redChannel   :: Double
                   , greenChannel :: Double
                   , blueChannel  :: Double
                   , alphaChannel :: Double
                   }
  deriving Show

{- where I have named the fields for documentation purposes. Each
   channel is only meant to take values between 0 and 1, and we'll
   have to be careful to make sure that it stays that way below.

   A colour image is now represented as a value of type:

      Picture RGBA

   Expanding out the definition of 'Picture', we see that a picture is
   a function from (x,y) coordinates to RGBA colour values.

   Let's give ourselves some colours: black, white, red, green, and
   blue. Note that all these colours have the alpha channel set to 1
   -- they are fully opaque. -}

black :: RGBA
black = MkRGBA 0 0 0 1

white :: RGBA
white = MkRGBA 1 1 1 1

red :: RGBA
red = MkRGBA 1 0 0 1

green :: RGBA
green = MkRGBA 0 1 0 1

blue :: RGBA
blue = MkRGBA 0 0 1 1

{- And the lack of colour (the alphaChannel is set to 0). -}

clear :: RGBA
clear = MkRGBA 0 0 0 0

{- And a way of modifying a colour's opacity: 'opacity f c' makes the
   colour 'c' transparent by a factor of 'f' (which should be between
   0 and 1). -}

opacity :: Double -> RGBA -> RGBA
opacity factor (MkRGBA r g b a) = MkRGBA r g b (a * factor)

{- So 'opacity 0.5 red' is half transparent red. -}

{- The first picture we'll make is one that 'green' everywhere: -}

greenEverywhere :: Picture RGBA
greenEverywhere (x,y) = green

{- See? 'greenEverywhere' is a function representing a picture that
   takes a coordinate (x,y) and always returns the colour 'green', no
   matter what the coordinate is. A slightly more complex picture is
   one that is blue when the x coordinate is less than 0, and green
   when it is greater or equal 0: -}

blueAndGreen :: Picture RGBA
blueAndGreen (x,y) = if x < 0 then blue else green

{- Making pictures like this and thinking about them is all very well,
   but it is much easier to see what is going on if we can look at the
   pictures we are creating. At the bottom of this file, I have
   defined a function 'writeBMP' that takes a filename and a 'Picture
   RGBA' and writes it to a file in the BMP format. Most image viewers
   will then be able to read this format and display it on screen.

   For example (in GHCi):

       *Ex2> writeBMP "test.bmp" blueAndGreen

   will write a file called "test.bmp" in the same directory as you
   started GHCi in. Opening this file in an image viewer will let you
   see the image. Values of type 'Picture RGBA' can represent very
   large images (up to the limits of the 'Double' type), so 'writeBMP'
   only takes the coordinates in the range (-100,99) in the x and y
   directions. The origin (0,0) is at the centre of the image. -}

{- Constructing pictures directly by writing a function from coordinates
   to colours is possible but difficult. It is much more fun to build
   pictures up by combining them together. -}


{- 2.3.0 Everywhere.

   Let's start by generalising the 'greenEverywhere' picture from
   above. Write a function that takes a value and returns that value
   at all coordinates. -}

everywhere :: a -> Picture a
everywhere = undefined

{- Test your function with 'writeBMP'. 'everywhere red' should generate
   a completely red image, for instance. -}

{- 1 MARK -}


{- 2.3.1 Shapes.

   To draw shapes, we won't do them using colours directly. Instead,
   we will create 'masks' that we will use to 'cut' shapes out of
   other pictures. As mentioned above, a mask is a 'Picture Bool'. We
   will describe some basic shapes using masks. For example, here is a
   function that generates a circular mask of a given radius: -}

circle :: Double -> Picture Bool
circle r = \(x, y) -> x*x + y*y <= r*r

{- So 'circle r' assigns 'True' to all points within distance 'r' of the
   origin, and 'False' otherwise. -}

{- Define a function rectangle that takes a width 'w' and a height 'h'
   and returns a picture assigning 'True' to all coordinates within
   the rectangle of width 'w' and height 'h' centred on the origin,
   and 'False' outside the rectangle. Note that the maximum distance
   along the x-axis from the origin is *half* the width, and similar
   for the height. -}

rectangle :: Double -> Double -> Picture Bool
rectangle = undefined

{- SCHEME: 1 mark for a correct definition. Half a mark off for making
   the rectangle twice as large as it ought to be. -}

{- 1 MARK -}


{- 2.3.2 Boolean operations on Pictures.

   Define the function 'pictureAND' that generates a new picture of
   'Bool's, where the boolean for a point is the "and" (&&) of the
   booleans at the same point in the two input pictures. Similarly,
   define 'pictureOR' and 'pictureNOT'. -}

pictureAND :: Picture Bool -> Picture Bool -> Picture Bool
pictureAND = undefined

pictureOR :: Picture Bool -> Picture Bool -> Picture Bool
pictureOR = undefined

pictureNOT :: Picture Bool -> Picture Bool
pictureNOT = undefined

{- 2 MARKS -}


{- 2.3.3 Drawing a Doughnut.

   Use the boolean operations you defined in the previous question to
   define a 'ring' shape: a circle of radius 100 with a circle of
   radius 50 cut out from the middle. -}

doughnut :: Picture Bool
doughnut = undefined


{- 1 MARK -}


{- 2.3.4 Colouring in.

   The shape functions don't return 'Picture's with RGBA colours, so
   we can't use 'writeBMP' to look at them directly. We have to
   translate 'Bool's to actual colours.

   Write a function that takes a 'Bitmap Bool' and two colours and
   produces a 'Bitmap RGBA' that uses the first colour when the mask
   is 'True' and the second when it is 'False': -}

colourIn :: Picture Bool -> RGBA -> RGBA -> Picture RGBA
colourIn = undefined

{- For example,

     *Ex2> writeBMP "test.bmp" (colourIn (circle 100) green black)

   should give a green circle on a black background when you load the
   file "test.bmp" into an image viewer.

     *Ex2> writeBMP "doughnut.bmp" (colourIn doughnut blue black)

   should give a blue doughnut on a black background in the file
   "doughnut.bmp". -}

{- 1 MARK -}


{- 2.3.5 Transforming images, point-by-point.

   'colourIn' is an example of a function that transforms a picture in
   a fixed way at every point. This is a pattern that happens over and
   over, so it is worth making a higher order function that captures
   the essence of this pattern.

   Define 'mapPicture' that takes a function 'f' from 'a's to 'b's and
   a Picture of 'a's and produces a Picture of 'b's by applying the
   function 'f' at every point. -}

mapPicture :: (a -> b) -> Picture a -> Picture b
mapPicture = undefined

{- It should be the case that:

      mapPicture (\b -> if b then green else black) (circle 100)

   produces the same image as

      colourIn (circle 100) green black

   Test this with 'writeBMP'. -}

{- 2 MARKS -}


{- 2.3.6 Transforming two images, point-by-point.

   'mapPicture' is useful, but sometimes we want to be able to apply a
   two argument function to two pictures simultaneously. Define a
   function 'mapPicture2' that for each point 'pt' uses the given
   function to combine the values of the two given pictures at 'pt': -}

mapPicture2 :: (a -> b -> c) -> Picture a -> Picture b -> Picture c
mapPicture2 = undefined

{- 1 MARK -}


{- 2.3.7 Varying the function, point-by-point.

   We could now go on and define mapPicture3, mapPicture4 and so on
   for combining more and more pictures.

   Instead of doing that, we can define a single function that can be
   used repeatedly. If we allow the function being used to transform
   the image to vary, as well as the argument, then we have a much
   more flexible arrangement. Define a function that takes a picture
   of /functions/ and a picture of /arguments/ and applies the
   functions to the arguments at each point. -}

pictureApply :: Picture (a -> b) -> Picture a -> Picture b
pictureApply = undefined

{- 2 MARKS -}



{- Now we can implement 'mapPicture' using 'everywhere' and
   'pictureApply':

      mapPicture f pic = everywhere f `pictureApply` pic

   The 'everywhere f' makes a bitmap that has the function 'f' at
   every point, and 'pictureApply' applies that function to the value
   of 'pic' at every point. Try writing out the definition of
   'pictureApply' and 'everywhere' as they are used in this definition
   of 'mapPicture' to see how it works. -}


{- 2.3.8 Re-implementing mapPicture2.

    Re-implement 'mapPicture2' using **only** 'everywhere' and
    'pictureApply'. You should not need to mention points anywhere.

    HINT: follow the types! remember that giving a function that takes
    two arguments one argument returns a function expecting the other
    argument. -}

mapPicture2' :: (a -> b -> c) -> Picture a -> Picture b -> Picture c
mapPicture2' = undefined

{- 1 MARK -}


{- Blending. Since we are representing colours with alpha channels for
   transparency, we can overlay one picture on top of another, letting
   the background picture show through the transparent bits of the
   foreground picture. We represent this as the ability to blend RGBA
   colours together. RGBA colours with alpha blending form a monoid
   (Week 05): we have the completely clear colour 'RGBA 0 0 0 0' and
   the monoid operation is alpha blending. The exact details of alpha
   blending are not important here. See the following blog post for a
   derivation of the definition from first principles:

     https://lukepalmer.wordpress.com/2010/02/05/associative-alpha-blending/

   Since we have an associative operation on RGBA colours, we are
   justified in declaring RGBA an instance of the Semigroup typeclass
   (Week 05), allowing us to combine colours: -}
instance Semigroup RGBA where
  (MkRGBA r1 g1 b1 0)  <> (MkRGBA r2 g2 b2 0)  = mempty
  (MkRGBA r1 g1 b1 a1) <> (MkRGBA r2 g2 b2 a2) = MkRGBA r g b a
    where
      a = a1 + a2 - a1*a2
      r = (a1*r1 + (1-a1)*a2*r2) / a
      g = (a1*g1 + (1-a1)*a2*g2) / a
      b = (a1*b1 + (1-a1)*a2*b2) / a

{- We can now write 'colour1 <> colour2' to blend colour1 and
   colour2. For example, blending 'red' and 'green' gives us just red:

       *Ex2> red <> green
       MkRGBA {redChannel = 1.0, greenChannel = 0.0, blueChannel = 0.0, alphaChannel = 1.0}

   because the "top" colour red is fully opaque. If we lower its
   opacity, then we get a mixture of red and green:

       *Ex2> opacity 0.5 red <> green
       MkRGBA {redChannel = 0.5, greenChannel = 0.5, blueChannel = 0.0, alphaChannel = 1.0}

   A Monoid is a Semigroup with a "zero" element. The "zero" for
   colours is the completely transparent colour: -}
instance Monoid RGBA where
  mempty = MkRGBA 0 0 0 0
  {- By default, mappend = (<>) -}


{- 2.3.9 Blending pictures.

   Use the '<>' function on any Semigroup and 'mapPicture2' to write a
   function that combines two images. We call this function 'over'
   because it is used to place one picture over another, letting the
   background picture show through the transparent parts of the
   foreground picture. -}

over :: Semigroup a => Picture a -> Picture a -> Picture a
over = undefined

{- 1 MARK -}


{- 2.3.10 Cutting out pictures.

   A more useful variant of the 'colourIn' function is one that takes
   a mask (a 'Bitmap Bool') and a image (a 'Bitmap a') and wherever
   the mask is 'True' uses the image, and wherever the mask is 'False'
   uses the 'mempty' of the monoid. When we use the Monoid structure
   on RGBA, this will correspond to leaving the cut-out parts
   transparent. We can then set a background by putting the resulting
   picture 'over' a background.

   Define the 'cut' function, using 'mapPicture2': -}

cut :: Monoid a => Picture Bool -> Picture a -> Picture a
cut = undefined

{- For example,

      circle 50 `cut` everywhere red

   will produce a red circle of radius 50 on a transparent background.

      (circle 50 `cut` everywhere red) `over` everywhere blue

   will produce a red circle of radius 50 on a blue background. -}

{- 1 MARK -}



{- 2.3.11 Space Transformations.

   All the functions so far have concentrated on transforming pixel
   values individually. Another class of transformations is to adjust
   the coordinates. This allows for rotates, scaling, shearing,
   flipping of images and so on.

   We can represent an arbitrary coordinate transformation as a
   function of type 'Point -> Point'. Here are some point
   transformation functions that perform translation and rotation: -}

translatePoint :: (Double,Double) -> Point -> Point
translatePoint (vx,vy) (x,y) = (x-vx, y-vy)

-- angle in radians
rotatePoint :: Double -> Point -> Point
rotatePoint angle (x,y) = ( x * cos angle - y * sin angle
                          , x * sin angle + y * cos angle)

scalePoint :: Double -> Point -> Point
scalePoint f (x,y) = (x/f, y/f)

{- Write a function that transforms a picture by the given
   transformation: -}

transform :: (Point -> Point) -> Picture a -> Picture a
transform = undefined

{- For example, 'transform (rotate pi/2) (rectangle 100 200)' will
   generate a picture (of booleans) that is a rectangle rotated by
   'pi/4' radians (45 degrees). -}

{- 1 MARK -}

{- 2.3.13 Flipping Pictures

   Write functions that flip a 'Picture'. The first function should
   flip top to bottom (and bottom to top). The second should flip left
   to right (and right to left).  Use 'transform' to write your
   functions. -}

flipTopBottom :: Picture a -> Picture a
flipTopBottom = undefined

flipLeftRight :: Picture a -> Picture a
flipLeftRight = undefined

{- 2 MARKS -}


{- With some point transformation functions, we can now create some
   "interesting" pictures, such as this spiral: -}

-- Three convenient functions for building
fade :: Picture RGBA -> Double -> Picture RGBA
fade pic f = mapPicture (opacity (1-f)) pic

at :: Picture a -> Point -> Picture a
at pic p = transform (translatePoint p) pic

scale :: Picture a -> Double -> Picture a
scale pic f = transform (scalePoint f) pic

picture :: Picture RGBA
picture =
  spiral 50 (purpleCircle `scale` 0.5)
  `over`
  everywhere black
  where
    spiral radius pic =
      mconcat [ pic `fade` fadeFactor `at` (dist * sin angle, dist * cos angle)
              | step <- [0..30]
              , let angle = (step / 10) * 2 * pi
                    dist  = (step / 12.5) * radius
                    fadeFactor = step / 30
              ]

    purpleCircle = circle 50 `cut` redBlueGradient

    redBlueGradient (x, y) = opacity d red <> blue
      where d = (x+100) / 200

{- See also 'Ex2Main' for another picture. You can run this by doing:

     stack build
     stack exec Ex2

   If you have filled in the functions above, you will get a
   'face.bmp' file to look at. -}

{----------------------------------------------------------------------}
{- PART 4 : PROCESSES                                                 -}
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


{- 2.4.0 'input'.

   'input' is the process that inputs a single value and then ends
   with that value. Write it.

      > runIO input
      foop                 -- typed by the user
      "foop"
-}

input :: Process x x
input = undefined

{- 1 MARK -}



{- 2.4.1 'output'.

   'output x' is the process that outputs the value x, and then ends
   with the value ().

      > runIO (output "Hello!")
      Hello!
-}

output :: x -> Process x ()
output x = undefined

{- 1 MARK -}


{- 2.4.2 Upgrading of Binary Processes.

   Here is the 'Process' type from Exercise 1, renamed to prevent a
   clash with our new more general 'Process' type. -}

data BProcess
  = BEnd
  | BOutput Bool BProcess
  | BInput BProcess BProcess
  deriving Show

{- Write a function that translates a 'BProcess' into a 'Process Bool
   ()'. Whenever the 'BProcess' ends, the 'Process Bool ()' process
   should end; whenever the 'BProcess' outputs a bit 'b', the 'Process
   Bool ()' process should output 'b'; and whenever the 'BProcess'
   inputs, the 'Process Bool ()' process should input, and do whatever
   the BProcess did.

   In the other direction, write a function that translates a 'Process
   Bool ()' to a 'BProcess'. -}

bprocessToProcess :: BProcess -> Process Bool ()
bprocessToProcess = undefined

processToBProcess :: Process Bool () -> BProcess
processToBProcess = undefined

{- 4 MARKS -}



{- 2.4.3 Sequential composition of processes.

   In the previous exercise, sequential composition of processes had
   type 'Process -> Process -> Process'. Here, processes terminate
   with a value, which is passed on to subsequent processes. Define
   the rest of this function to complete the definition of sequential
   composition of processes.

   Here are some examples of its use:

       > runIO (input `sequ` \x -> output x)
       hello
       hello

       > runIO (input `sequ` \x -> End ())
       hello

   Note that using the the backtick notation to write 'sequ' between
   its arguments allows us to read 'p1 `sequ` \x -> p2' as "do 'p1',
   call the result 'x' and then do 'p2'". -}

sequ :: Process x a -> (a -> Process x b) -> Process x b
sequ = undefined

{- 3 MARKS -}


{- 2.4.4 Define a process that does the same thing as 'echo' above but
   only using 'input', 'output' and 'sequ'. -}

echoFromSequ :: Process x ()
echoFromSequ = undefined

{- 1 MARK -}


{- 2.4.5 A chat bot

   Write a value of type 'Process String ()' that when run through
   'runIO' produces the following interaction:

      > runIO chatBot
      Hello! What is your name?          <-- this is output
      Haskell                            <-- this is what the user types
      Hello Haskell!                     <-- this is output

   If the user enters a different name, then the final line should use
   that name instead of 'Haskell'.

   It is possible to write 'chatBot' directly using 'Output' and
   'Input', but there is a tidier approach using 'sequ' as an infix
   function between its arguments.

   To construct your response message, remember that Haskell 'String's
   are lists and you can concatenate them using '++'. -}

chatBot :: Process String ()
chatBot = undefined

{- 4 MARKS -}


{- 3.4.8 Testing Equality of Processes.

   Let's say that we want to write a function that tests two processes
   for equality -- that is they do the same input and output
   operations, and end with the same values, for all possible
   inputs. We want to write a function of type:

      eqProcess :: Process x () -> Process x () -> Bool

   Such a function would be very helpful for debugging processes. For
   example, we could discover that there is more than one way of
   writing the same process:

      eqProcess echo (input `sequ` output) == True

   Unfortunately, it is not possible in general to write such a
   function that compares for equality. Roughly speaking, this is
   because if the type of inputs 'x' has infinitely many possible
   values, there is no way to test that both processes handle all
   infinitely many values in the same way.

   However, if we restrict the possible values that are input and
   output by the process to be a type with only finitely many values,
   for example 'Bool' only has the values 'True' and 'False', then we
   can decide equivalence between two processes.

   Complete the missing cases (the bits that are 'undefined') in the
   following function definition to write a function that decides
   equality of two processes. -}

eqProcess :: Process Bool () -> Process Bool () -> Bool
eqProcess (End ())       (End ()) =
  undefined  -- are these processes equal?
eqProcess (Output b1 k1) (Output b2 k2) =
  undefined
  -- do they output the same thing, and do they
  -- do the same thing after that?
eqProcess (Input k1)     (Input k2) =
  undefined
  -- what are the possible input values of type Bool?
eqProcess _ _ =
  False
  -- if they don't agree on 'End'ing, 'Input'ing or
  -- 'Output'ing, then they aren't equal


{- Try it on the 'echo' process. The following example is one that
   should return 'False':

       eqProcess echo (input `sequ` (\_ -> End ())) == False

   While the rewrite of 'echo' ought to return 'True':

       eqProcess echo (input `sequ` output) == True
-}

{- 5 MARKS -}



{----------------------------------------------------------------------}
{- END OF EXERCISE                                                    -}
{----------------------------------------------------------------------}

{----------------------------------------------------------------------}
{- APPENDIX                                                           -}
{----------------------------------------------------------------------}

{- Below are the functions implementing BMP file output. These are used
   by Part 3 of the exercise above. -}


{- 'writeBMP filename bitmap' samples 'bitmap' for the pixels in the
   range ((-100,99),(-100,99)) and outputs them as a BMP file with the
   given filename. It uses the 'buildBMP' function defined below to
   construct a 'ByteString Builder' object that describes the stream
   of bytes to write to the file. -}
writeBMP :: FilePath -> Picture RGBA -> IO ()
writeBMP filename bitmap = do
  h <- openFile filename WriteMode
  hPutBuilder h (buildBMP 200 200 bitmap)
    `finally` hClose h

{- 'buildBMP width height bitmap' returns a ByteString Builder
   containing the pixels sampled from 'bitmap' around the origin in
   the Windows BMP file format in 8 bits per channel with an 8 bit
   alpha channel. The file format details were taken from here:

      https://en.wikipedia.org/wiki/BMP_file_format#Example_2

   The file format is relatively simple: there is a header describing
   the image (size, resolution, colour layout), followed by the pixel
   data. We are not using any compression. Most of the header is '0'
   because we are just relying on the defaults for colour space
   correction and gamma.

   The 'LE' suffixes on all the word16/32 calls signify that BMP is a
   'little endian' format, as would be expected from its origins on
   Intel x86 systems.

   The sampling and quantization of the bitmap are quite naive. Taking
   the average of surrounding pixels would probably produce "more
   correct" images. -}
buildBMP :: Word32 -> Word32 -> Picture RGBA -> Builder
buildBMP width height bitmap = header <> pixelData
  where
    headerSize    = 122
    pixelDataSize = height * width * 4
    fileSize      = headerSize + pixelDataSize

    header =
      fold [ word8 0x42, word8 0x4d -- "BM"
           , word32LE fileSize
           , word16LE 0             -- application specific
           , word16LE 0             -- application specific
           , word32LE headerSize    -- offset to the pixel data
           , word32LE 108           -- DIB header size
           , word32LE width
           , word32LE height
           , word16LE 1             -- 1 colour plane
           , word16LE 32            -- 32 bits per pixel
           , word32LE 3             -- "BI_BITFIELDS" format, no compression
           , word32LE pixelDataSize
           , word32LE 2835          -- horizontal resolution: 2835 ppm (72 DPI)
           , word32LE 2835          -- vertical resolution: 2835 ppm (72 DPI)
           , word32LE 0             -- 0 colours in the palette (not using one)
           , word32LE 0             -- 0 "important" colours
           , word32LE 0x00ff0000    -- red channel bitmask
           , word32LE 0x0000ff00    -- green channel bitmask
           , word32LE 0x000000ff    -- blue channel bitmask
           , word32LE 0xff000000    -- alpha channel bitmask
           , word32LE 0x57696e20    -- "Win " for LCS_WINDOWS_COLOR_SPACE
           , fold (replicate 0x24 (word8 0)) -- CIE colour space endpoints (unused)
           , word32LE 0             -- red gamma (unused)
           , word32LE 0             -- green gamma (unused)
           , word32LE 0             -- blue gamme (unused)
           ]

    pixelData =
      fold [ encode (bitmap (pixelToBitmap x y))
           | y <- [0..height-1]
           , x <- [0..width-1]
           ]

    -- coordinate space transformations
    pixelToBitmap x y =
      ( fromIntegral x - (fromIntegral width / 2)
      , fromIntegral y - (fromIntegral height / 2))

    -- pixel encoding, little endian
    encode (MkRGBA r g b a) = foldMap quantize [ b, g, r, a ]
    quantize v = word8 (round (255 * v))
