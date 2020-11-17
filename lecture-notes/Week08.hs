{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Week08 where

import Data.Char          (toUpper, isDigit, digitToInt, isSpace)
import Data.Foldable      (for_)
import Data.IORef         (IORef, newIORef, readIORef,
                           writeIORef, modifyIORef)
import Control.Exception  (finally)
import System.IO          (openFile, hPutChar, hGetChar,
                           hClose, IOMode (..), hIsEOF, Handle)


{-    WEEK 8 : REAL I/O and PARSER COMBINATORS -}



{-    Part 8.1 : I/O Conceptually

   Beginning in Week 06, we've seen how to simulate side effects in
   Haskell. But what if we want to do some *real* side effects?

   A great philosopher once wrote:

     The philosophers have only interpreted the world, in various
     ways. The point, however, is to change it.

      -- Karl Marx ( https://en.wikipedia.org/wiki/Theses_on_Feuerbach )

   So how do we change the world, but remain with a language that only
   allows functions with no side effects?

   To see why adding side effects to Haskell is not entirely
   straightforward, imagine that we added a function that prints a
   character from the input and returns an empty tuple. It might have
   the have the type:

       putChar :: Char -> ()

   But adding this function has a large downside. It would spoil our
   ability to reason by replacing equals by equals.

   For example, in the code:

      f x = (x + 100, x + 100)

   we should be able to think "there is duplicated code here, let's
   make a definition that removes this duplication". Like this:

      f x = (z, z) where z = x + 100

   However, if we have the following code

      f x = (putChar x, putChar x)

   Then we might expect that this outputs the character passed in via
   the variable 'x' twice. However, if we do the same transformation
   to reduce duplicate code:

      f x = (z, z) where z = putChar x

   it is not clear whether calling 'f' should output two characters or
   one. Having functions that perform side effects reduces our ability
   to rearrange programs by replacing equals by equals.

   To resolve this tension, Haskell treats side effects (such as
   printing) not as things that happen as a "side effect" of calling a
   function, but as a *value* that we construct. Recall that in Week
   06 we saw the use of simulations of mutable state and printing,
   representing the idea of side effects with normal data. We can
   think of the value as a description of the actions we want to
   perform. The type of these descriptions is 'IO a', where 'a' is the
   type of values that executing the action will return. Therefore,
   'putChar' has the type:

       putChar :: Char -> IO ()

   That is, 'putChar' doesn't actually do the printing when you call
   it. Instead, it returns an "IO action". So the following code:

       f x = (putChar x, putChar x)

   doesn't output two characters, instead it returns a pair of two IO
   actions, that, when they are executed, output whatever is in
   'x'. Because 'putChar x' is an IO action, not an instruction to
   output, the rewritten code:

       f x = (z, z) where z = putChar x

   Has exactly the same meaning, and we have retained the ability to
   always replace equals by equals in code.

   Conceptually, we can think of such "actions" as being in a datatype
   similar to the 'Process' type from Exercise 2: -}

data IOAction a
  = End a
  | Input (Char -> IOAction a)
  | Output Char (IOAction a)

{- If actions are values of this datatype, we can pass them around
   between functions, place them inside larger data structures, and
   combine them in various ways, all while retaining the ability to
   reason about our programs using replacement of equals by
   equals. Only when an action is passed to the Haskell runtime system
   does it actually get executed.

   The main interface we use for combining values of type 'IOAction a'
   (and 'IO a') is the Monad interface we saw last week. In each of
   the instances of 'Monad' we've seen so far, we've isolated the idea
   of "do some side effects and possibly return a value" into
   different types according to the different kinds of side
   effects. The '>>=' function is exactly the 'sequ' function in
   Exercise 2, which sequences the actions of the first argument
   before the second one.

   The real IO monad is not actually implemented like this. The main
   difference between 'IOAction' and the real 'IO' monad is that we
   cannot pattern match on values of type 'IO a'. This allows the
   Haskell system to more efficiently optimise and execute IO actions.

   In summary, Haskell remains a "pure" language, and allows side
   effects by:

     1. Having a special type 'IO a' of I/O actions that peform some
        "real world" actions and return values of type 'a'.

     2. Using the 'Monad' interface to combine individual 'IO a'
        actions into sequences.

     3. Actually executing an 'IO a' action happens either by typing
        its name at the prompt in GHCi, or in a standalone program by
        being whatever action is defined to be the 'main' value.

   In the next sections, we'll look at some of the basic operations
   that the IO monad has, and how they can be put together to write
   programs that interact with the outside world. -}


{-    Part 8.2 : Doing I/O in Haskell

   So 'IO a' is the type of "IO actions returning values of type
   'a'". But what are the primitive operations for 'IO'? The 'Maybe'
   monad has 'failure' and 'catch', the 'State' monad has 'get' and
   'put', and so on. What about 'IO'?

   'IO' has a very large number of operations, corresponding to all
   the different ways of interacting with the outside world. Covering
   all of the them is well outside the scope of this course, so I'll
   just cover a few to get the general idea.

   Perhaps the simplest is 'putChar', which has the following type:

      putChar :: Char -> IO ()

   As the name indicates, it puts a single character to the output. We
   can try it in GHCi:

      > putChar 'x'
      x>

   GHCi treats values of type 'IO a' specially: instead of printing
   out the value of type 'IO a', it executes the actions described by
   this value, and then prints out the final value.

   Notice that there is no newline between the 'x' and the next prompt
   from GHCi.

   Since 'IO' is a 'Monad', we can use the "do notation" described in
   Week 07 with it, and print two characters, one after the other: -}

putTwoChars :: Char -> Char -> IO ()
putTwoChars c1 c2 =
  do putChar c1
     putChar c2

{- For example:

      > putTwoChars 'h' 's'
      hs>

   It is important to emphasise what is happening here: "putTwoChars
   'h' 's'" builds an I/O action that (when run) outputs the
   characters 'h' and 's'. It is not actually executed until we give
   that action to GHCi.

   Using the generic 'for_' function from Week 07 that iterates some
   action for every element of a list, we can write a function that
   writes a character a fixed number of times to the output: -}

writeN :: Int -> Char -> IO ()
writeN n c = for_ [1..n] (\_ -> putChar c)

{- For example:

      > writeN 20 'a'
      aaaaaaaaaaaaaaaaaaaa>

   More usefully, we can write a function that iterates through a
   'String' (which is a list of characters), running putChar on each
   one, and then outputs a newline character: -}

printLine :: String -> IO ()
printLine xs =
  do for_ xs (\x -> putChar x)
     putChar '\n'

{- For example:

      > printLine "Hello world"
      Hello world

   This function is already in the standard library as 'putStrLn',
   which knows more about OS-specific line endings.

   There is also a primitive "read a character" operation:

      getChar :: IO Char

   From the type, 'getChar' is an IO operation that returns a
   'Char'. For example:

      > getChar
      x                   <-- this is entered by the user
      'x'                 <-- this is printed by GHCi

   Note that you'll have to press 'Enter' after typing a character for
   the terminal to actually send the character to GHCi, but the
   'Enter' is not returned by 'getChar'.

   As with 'putChar', we can use the 'Monad' operations of 'IO' to
   sequence uses of 'getChar': -}

getTwoChars :: IO (Char, Char)
getTwoChars =
  do c1 <- getChar
     c2 <- getChar
     return (c1, c2)

{- For example:

      > getTwoChars
      xy                 <-- entered by user
      ('x','y')          <-- printed by GHCi

   (again, you'll need to press 'Enter' after the two characters to
   make the terminal actually send the input.)

   To read lines of input, we need to keep reading characters until we
   read a newline ('\n'). The following function does this: -}

readLine :: IO String
readLine =
  do c <- getChar
     if c == '\n' then
       return []
     else
       do cs <- readLine
          return (c:cs)

{- The following function does the same thing, except that it uses an
   accumulator to build up the list of characters from the input, and
   reverses it at the end. This function consumes less stack space
   than 'readLine'. -}

readLine2 :: IO String
readLine2 = go []
  where go accum =
          do c <- getChar
             if c == '\n' then
               return (reverse accum)
             else
               go (c:accum)

{- Either way, you probably shouldn't implement this function yourself,
   because it is already in the standard library as 'getLine'. The
   standard library version also knows more about OS-specific line
   endings.

   Now that we have the ability to read and write whole lines, we can
   write simple functions that interact with the user. The classic
   "What is your name?" function: -}

program :: IO ()
program =
  do printLine "Hello, what is your name?"
     name <- readLine
     printLine ("Hello " ++ name ++ "!")

{- Example run:

     > program
     Hello, what is your name?
     Bob
     Hello Bob!

   A function that repeatedly reads input until it gets an empty line,
   outputing non-empty lines in CAPITALS: -}

capsLockSimulator :: IO ()
capsLockSimulator =
  do line <- readLine
     if null line then return ()
       else do printLine (map toUpper line)
               capsLockSimulator

{- An example interaction:

     > capsLockSimulator
     hello
     HELLO
     no, really, i am being calm
     NO, REALLY, I AM BEING CALM

     >
-}



{-    Part 8.3 : File Input and Output

   The 'putChar' and 'getChar' functions above allow us to read from
   the standard input and write to the standard output. But what if we
   want to read and write to specific files?

   Just as in many other languages, Haskell provides facilities for
   opening files to get file handles, which can be given to read and
   write function, and then finally closed. The basic API for this is
   as follows:

   File paths are represented as 'String's, using a type synonym:

       type FilePath = String

   When we open files, we have to say what we're going to do with it:
   read, write, append to the end, or random access reading and
   writing:

       data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode

   After we open a file, we get back a 'Handle', which is another
   abstract type:

       type Handle

   The function to actually open files is 'openFile' which takes a
   'FilePath' and an 'IOMode' and returns an 'IO' action that will
   yield a 'Handle' (or throw an exception if it can't open the file,
   see below):

       openFile :: FilePath -> IOMode -> IO Handle

   To read and write individual characters from a 'Handle', we use
   variants of the 'getChar' and 'putChar' functions we saw above:

       hGetChar :: Handle -> IO Char

       hPutChar :: Handle -> Char -> IO ()

   Finally, when we are done with a file, we 'hClose' the 'Handle' to
   return the associated resources to the operating system:

       hClose   :: Handle -> IO ()

   Let's see those functions in action. Here is a function that writes
   a 'String' to a file (called 'writeFile' in the standard
   library). It opens the specified file in 'ReadMode', uses a 'for_'
   loop to write every character from 'string' into the handle, and
   then closes the handle. -}

writeToFile :: FilePath -> String -> IO ()
writeToFile path string =
  do handle <- openFile path WriteMode
     for_ string (\x -> hPutChar handle x)
     hClose handle

{- Note that this function is not "exception safe": if 'hPutChar' throws
   an exception (e.g., the disk is full), then the file won't be
   closed properly. We'll see how to fix this below.

   To read an entire file into a 'String', we'll need to keep reading
   until the end of the file. To find out if a 'Handle' is at the end
   of a file (EOF), we can use the function 'hIsEOF':

      hIsEOF :: Handle -> IO Bool

   We can now write a function that opens a file, reads characters
   until we get to EOF and returns all the characters read as a
   'String', closing the file before it returns: -}

readFromFile :: String -> IO String
readFromFile path =
  do handle <- openFile path ReadMode
     content <- go handle ""
     hClose handle
     return content
  where
    go h content =
      do isEOF <- hIsEOF h
         if isEOF then
           return (reverse content)
         else
           do c <- hGetChar h
              go h (c:content)

{- This is a toy implementation that is quite slow because it reads in
   the data one character at a time. You should use the Haskell
   standard library function 'readFile' to actually read in files.

   Above, I mentioned that these functions are not "exception safe" in
   the sense that if one of the I/O operations throws an exception,
   then any open files will not be close properly, and there will be a
   resource leak.

   A proper discussion of handling of IO exceptions in Haskell
   (distinct from the 'simulated' exceptions we saw in Week 06) is
   beyond the scope of this course. However, I'll just how how to fix
   this problem.

   Haskell provides a function 'finally' that is similar to the
   'finally' blocks in Java. It has the type:

       finally  :: IO a -> IO b -> IO a

   The idea is that

       finally action cleanup

   Is an IO action that performs 'action'. Then, however 'action'
   terminates, either normally, or by throwing an exception, 'cleanup'
   is performed. The whole action then either terminates with a value
   or rethrows the exception.

   We can use finally to write a higher order function that opens a
   file and then passes the handle to the given 'body', using
   'finally' to close the file no matter how 'body' terminates. -}

withInputFile :: FilePath -> (Handle -> IO a) -> IO a
withInputFile path body =
  do handle <- openFile path ReadMode
     result <- body handle `finally` hClose handle
     return result

{- With this function, we can rewrite 'readFromFile' to be exception
   safe. Now, if 'hIsEOF' or 'hGetChar' throw an exception, the file
   will still be safely closed. -}

readFromFile_v2 :: FilePath -> IO String
readFromFile_v2 path =
  withInputFile path (\handle -> go handle "")
  where
    go h content =
      do isEOF <- hIsEOF h
         if isEOF then
           return (reverse content)
         else
           do c <- hGetChar h
              go h (c:content)


{-    Part 8.4 : PARSER COMBINATORS

   Once we can read in files, we need to be able to understand
   them. Conversion of data from a string of characters to a
   structured representation is called "parsing". In some sense,
   parsing in some form or another is pretty much all of what
   computers are asked to do, and there are many different ways of
   doing it. The remainder of this week is about a particular way of
   doing parsing using functional programming ideas.

   We saw a simple example of parsing in Exercise 1: 'splitOn' splits
   a "flat" string of fields separated by some character into the
   separate fields. We can use 'splitOn' to parse lines from CSV
   (Comma Separated Values) files:

       > splitOn ',' "CS316,Functional Programming,20"
       ["CS316", "Functional Programming", "20"]

   But 'splitOn' is a bit too simplistic. For example, how would we
   parse fields that contain commas? CSV files usually put fields that
   contain commas in quotes:

        "CS311,\"Programming Language, Design and Implementation\",20"

   which should be understood as:

        ["CS311", "Programming Language, Design and Implementation", "20"]

   But 'splitOn' will split this in the wrong way:

        ["CS311", "\"Programming Language", " Design and Implementation\"", "20"]

   Fixing this is an example of a parsing problem. -}

{- How should we write parsers in Haskell? One way to go about this is
   to have the following two thoughts:

   - Parsers take input that is a list of 'Char's, i.e., a 'String'.

   - Parsers can either fail to parse, because it recieved invalid
     input, or succeed, in which case it should return the structured
     representation.

   Thinking like this leads to the following definition ('version 1'): -}

type Parser_v1 a = String -> Maybe a

{- That is: a Parser of things of type 'a' is a function that takes
   'String's and either returns 'Nothing' if the input is malformed,
   or returns 'Just' of something if the input is wellformed.

   Let's try this idea with to parse 'Bool'ean values. We want to
   recognise the string "True" to return the value 'True', and the
   string "False" to return the value 'False'. The following function
   does this by using pattern matching: -}

parseBool_v1 :: Parser_v1 Bool
             -- String -> Maybe a
parseBool_v1 "True"  = Just True
parseBool_v1 "False" = Just False
parseBool_v1 _       = Nothing

{- This seems to work:

      > parseBool_v1 "True"
      True
      > parseBool_v2 "False"
      False

   But of course, we want to parse more complex input than just
   exactly "True" or "False". Could we use our parser to parse two
   booleans? We want to turn a string like:

      "TrueFalse"

   into the Haskell value:

      (True, False)

   But there seems to be no obvious way of doing this with our parsers
   as they are currently written. If we try to parse the string
   "TrueFalse" with 'parseBool_v1', it will fail because it is looking
   for exactly either the string "True" or the string "False".

   Wouldn't it be better to be able to reuse our parser for 'Bool's to
   parse a 'Bool', and then let something else parse the rest of the
   input?

   The reason we can't compose our parsers into larger parsers because
   the 'Parser_v1' type describes parsers that are "monolithic" --
   they make a decision about the whole input, and offer no way to
   pass what they don't understand on to another parser. We need
   modular parsers that can be chained together. The way we will do
   this is by making each parser responsible for parsing as much of
   the start of the input as it can, and then returning any left over
   input to be passed on to the next parser. -}


{- We upgrade our 'Parser_v1' to also return a 'String' on success
   like so: -}

newtype Parser a = MkParser (String -> Maybe (String, a))

{- A 'Parser' of things of type 'a' is a function that takes 'String's
   as input and either returns 'Nothing', or 'Just (leftover, a)',
   where 'leftover' is the remainder of the input that wasn't parsed,
   and 'a' is the value understood from the beginning of the
   input. We'll give some examples below.

   We've also wrapped the type definition in a 'newtype' so that we
   can define some typeclass instances for it below (similarly to how
   we needed to use 'newtype' for the 'State' monad in Week 07).

   To run a parser, we need a function that unwraps the 'newtype' and
   runs the underlying parser on an input: -}

runParser :: Parser a -> String -> Maybe (String, a)
runParser (MkParser p) input = p input

{- Now we can write a parser that parses 'Bool's by looking at the start
   of the input. If it sees either "True" or "False", it returns the
   'rest' of the input, and the appropriate value. Otherwise, it
   returns 'Nothing'. -}

parseBool_v2 :: Parser Bool
parseBool_v2 =
  MkParser (\input ->
              case input of
                'T':'r':'u':'e':rest     -> Just (rest, True)
                'F':'a':'l':'s':'e':rest -> Just (rest, False)
                _                        -> Nothing)

{- Let's try it:

      > runParser parseBool_v2 "True"
      Just ("", True)
      > runParser parseBool_v2 "False"
      Just ("", False)
      > runParser parseBool_v2 "True101010"
      Just ("101010", True)
      > runParser parseBool_v2 "Truthy"
      Nothing
      > runParser parseBool_v2 "FalseLEFTOVERS"
      Just ("LEFTOVERS", False)

   Notice that, in the case of a successful parse, the remainder of
   the input is returned along with the result of the parsing. We can
   use this to chain two parsers together, passing the leftover input
   from the first into the second.

   Here is a function that takes two 'Parser's, one for 'a's and one
   for 'b's, and creates a 'Parser' for pairs '(a,b)' using the
   following strategy:

     1. It takes the input in the variable 'input'.

     2. It runs the first parser on 'input'. If it fails (returns
        'Nothing') the combined parser returns 'Nothing'.

     3. If the first parser succeeds with leftovers 'input2' and
        result 'a', then it runs the second parser on 'input2'. If
        that fais, the combined parser returns 'Nothing'.

     4. If the second parser succeds with leftovers 'input3' and
     result 'b' then the combined parser returns leftovers 'input3'
     and the final result '(a,b)'.

   Here is the Haskell code for this strategy: -}

parsePair :: Parser a -> Parser b -> Parser (a,b)
parsePair p1 p2 =
  MkParser (\input -> case runParser p1 input of
                        Nothing ->
                          Nothing
                        Just (input2, a) ->
                          case runParser p2 input2 of
                            Nothing ->
                              Nothing
                            Just (input3, b) ->
                              Just (input3, (a,b)))

{- We can use 'parsePair' to parse two 'Bools', one after the other by
   calling 'parsePair' with 'parseBool_v2' for both arguments: -}

parsePairOfBools :: Parser (Bool, Bool)
parsePairOfBools = parsePair parseBool_v2 parseBool_v2

{- Some example runs:

      > runParser parsePairOfBools "TrueTrue"
      Just ("", (True, True))
      > runParser parsePairOfBools "FalseTrue"
      Just ("", (False, True))
      > runParser parsePairOfBools "FalseTrueLEFTOVERS"
      Just ("LEFTOVERS", (False, True))
      > runParser parsePairOfBools "FalsTru"
      Nothing

   Let's look at 'parsePair' in more detail.

   The cascade of 'case's with 'Nothing' always resulting in 'Nothing'
   is similar to way that we modelled exceptions using 'Maybe' in
   Lecture 10. Also, the passing and returning of the left over input
   from one parser to the next ('input', 'input2', and 'input3') looks
   very similar to how the state value was passed through when we
   modelled mutable state in Lecture 11.

   Both 'Maybe' and 'State' were instances of 'Monad', which allowed
   us to use generic tools for all 'Monad's, as we saw in Lecture 12.
   Could it be that 'Parser' is a 'Monad' too? Let's try to implement
   the 'Monad' interface: -}

instance Monad Parser where
  return x = MkParser (\input -> Just (input, x))
{- The 'return' for 'Parser's is very similar to the combination of
   'return' for 'Maybe', which always succeeded by returning 'Just x',
   and 'return' for 'State', which returned the state value (here
   'input') without modification. In terms of parsing, 'return x' is a
   parser which consumes no input and always succeeds. -}

  (>>=) :: Parser a -> (a -> Parser b) -> Parser b
  p >>= k =
    MkParser (\input ->
                case runParser p input of
                  Nothing ->
                    Nothing
                  Just (input2, a) ->
                    runParser (k a) input2)
{- '>>=' ("bind") for 'Parser's is also like a combination of the '>>='s
   for 'Maybe' and 'State'. It takes the initial input 'input' and
   passes it to the first 'Parser' 'p'. If that fails, then (like
   'Maybe') it returns 'Nothing'. If it succeeds with leftovers
   'input2' and result 'a', it calls 'k' with 'a' to get the
   continuation 'Parser' and runs it with 'input2'. -}

{- Now that we have defined 'Parser' to be an instance of 'Monad', we
   can use "do" notation with 'Parser's. Here is how we can parse
   pairs of 'Bool's using the 'parseBool' parser we wrote above: -}

parsePairOfBools_v2 :: Parser (Bool, Bool)
parsePairOfBools_v2 =
  do b1 <- parseBool_v2
     b2 <- parseBool_v2
     return (b1, b2)

{- In words: first we parse a boolean, call it 'b1', then we parse a
   boolean, call it 'b2'. Finally, we return the pair '(b1,b2)'. -}


{-    Part 8.5 : Building Parsers

   The definitions of the 'Monad' functions for 'Parser' form the
   foundation of how we're going to build complicated parsers by
   putting together simple parsers. The bind ('>>=') from the 'Monad'
   interface lets us put one parser after another to parse two things
   in sequence.

   The simplest parser we'll use is one that just parses a single
   character from the input. If there is a character in the input, it
   consumes it and returns it. Otherwise it fails. Here is the
   definition: -}

char :: Parser Char
char =
  MkParser (\input -> case input of
                        c:input1 -> Just (input1, c)
                        _        -> Nothing)

{- Some examples:

      > runParser char "hello"
      Just ("ello", 'h')
      > runParser char "ello"
      Just ("llo", 'e')
      > runParser char ""
      Nothing

   From these examples, we can see that 'char' only fails if the input
   is empty. Otherwise, it returns the first character in the input.

   It'll also be useful to have a 'Parser' that always fails (similar
   to 'failure' we defined for 'Maybe' in Week 06). We'll use this
   whenever we read a piece of input that we don't want. -}

failParse :: Parser a
failParse =
  MkParser (\input -> Nothing)

{- This parser always fails, no matter what input we give it:

      > runParser failParse ""
      Nothing
      > runParser failParse "hello"
      Nothing

   We can put the 'Parser's 'char' and 'failParse' together to write a
   parser that only succeeds on the character 'expected' we give
   it. It first uses 'char' to read a single character 'got'. If 'got'
   and 'expected' are the same thing, then it returns '()', signaling
   success. If they are not equal, it uses 'failParse' to reject this
   input. -}

isChar :: Char -> Parser ()
isChar expected =
  do got <- char
     if got == expected then return () else failParse

{- Some examples:

      > runParser (isChar 'h') "hello"
      Just ("ello", ())
      > runParser (isChar 'h') "ello"
      Nothing
      > runParser (isChar 'h') ""
      Nothing

   If we can write a parser that checks for specific characters, we
   can use it with a list of specific characters to check for specific
   strings of characters. We already have a function that performs
   some action for every element of a list: 'mapM_' from Lecture
   12. Using this, we get a parser that takes a specific 'String', and
   succeeds only if that string is at the start of the input: -}

isString :: String -> Parser ()
isString = mapM_ isChar

{- For example:

      > runParser (isString "hello") "hello"
      Just ("", ())
      > runParser (isString "hullo") "hello"
      Nothing
      > runParser (isString "hello") "hello world"
      Just (" world", ())


   Using 'char', we can write a parser for 'Bool's without having to
   write the underlying parser directly. We can read the first
   character and then decide whether to try to read a "True" or a
   "False": -}

parseBool_v3 :: Parser Bool
parseBool_v3 =
  do c <- char
     case c of
       'T' -> do isString "rue"
                 return True
       'F' -> do isString "alse"
                 return False
       _   -> failParse

{- This has the same behaviour as the 'parseBool_v2' we wrote above:

      > runParser parseBool_v3 "True"
      Just ("", True)
      > runParser parseBool_v3 "False"
      Just ("", False)
      > runParser parseBool_v3 "True101010"
      Just ("101010", True)
      > runParser parseBool_v3 "Truthy"
      Nothing
      > runParser parseBool_v3 "FalseLEFTOVERS"
      Just ("LEFTOVERS", False)

   This way of writing a parser for 'Bool'eans works, but it is a bit
   clumsy. What if we have a lot of words to recognise? What if a lot
   of them share the first few letters? Definitions like this would
   get quite messy.

   If we think back to Week 06 on using 'Maybe' to model exceptions,
   we had the function 'catch' for modelling the idea of "trying one
   thing, and if that fails, trying another". We could use something
   like this for writing 'Parser's: we try one parser (for parsing the
   word "True", say), and if that doesn't work try another (e.g., for
   parsing the word "False").

   We'll call the function that tries one parser and then another
   'orElse'. Its definition is very similar to the one for 'catch',
   except that we also pass the current 'input' into each parser: -}

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 =
  MkParser (\input ->
              case runParser p1 input of
                Nothing       -> runParser p2 input
                Just (rest,x) -> Just (rest,x))

{- Some examples, showing how it is used in an infix position to parse
   either an 'a' or a 'b' at the start of the input:

      > runParser (isChar 'a' `orElse` isChar 'b') "abc"
      Just ("bc", ())
      > runParser (isChar 'a' `orElse` isChar 'b') "bca"
      Just ("ca", ())
      > runParser (isChar 'a' `orElse` isChar 'b') "cab"
      Nothing

   We can also see the fail-and-try-the-other-one behaviour directly
   if we try 'failParse' or else something:

      > runParser (failParse `orElse` isChar 'a') "abc"
      Just ("bc", ())


   With the ability to try one parser and fall back to another if it
   fails, we can write parsers that are built from several
   possibilities, and return different results from according to what
   happened. For example, here is a parser that first tries to match
   the input with "True", if that works then it returns 'True';
   otherwise it tries to match the input with "False" and if that
   works it returns 'False': -}

parseBool :: Parser Bool
parseBool =
  do isString "True"
     return True
  `orElse`
  do isString "False"
     return False

{- This has the same behaviour as 'parseBool_v2' and 'parseBool_v3' we
   wrote above:

      > runParser parseBool "True"
      Just ("", True)
      > runParser parseBool "False"
      Just ("", False)
      > runParser parseBool "True101010"
      Just ("101010", True)
      > runParser parseBool "Truthy"
      Nothing
      > runParser parseBool "FalseLEFTOVERS"
      Just ("LEFTOVERS", False)

   An important point about 'orElse' is that it represents *ordered*
   choice. If the first parser succeeds, then its result is used, even
   if the second one could have succeeded. For example, the following
   parser built from 'orElse' will greedily match "Four", and never
   let the other parser have a go: -}

orderedChoiceDemo :: Parser String
orderedChoiceDemo =
  do isString "for"
     return "the keyword 'for'"
  `orElse`
  do isString "forty"
     return "the number 40"

{- We can see this behaviour in some examples:

     > runParser orderedChoiceDemo "for"
     Just ("","the keyword 'for'")
     > runParser orderedChoiceDemo "forty"
     Just ("ty","the keyword 'for'")

   On the input "forty", the first parser for "for" matches, and then
   we get the result "the keyword 'for'" and leftover input "ty". The
   second parser never gets to run.

   This ordered behaviour is often useful, because it gives us unique
   (unambiguous) answers if there are multiple ways of parsing the
   same input. However, it does mean that the 'Parser's we write might
   not parse the input in the way that we intend. It is always a good
   idea to have a collection of test inputs for testing your
   'Parser's. -}

{- We now have the following basic 'Parser's and ways of combining
   them:

     1. The 'char' 'Parser' for parsing single characters.

     2. The monad interface 'return' and '>>=' for parsers that do
        nothing and sequence two parsers.

     3. The 'failParse' parser that always fails.

     4. The 'orElse' parser that tries one parser, and if that fails
        tries another.

   With these basic parsers, we have built 'isString' that recognises
   literal strings. And we can now go on to build more useful parsers
   on top of these.

   With these basic functions, we no longer have to write anything
   involving 'MkParser' directly. All the other parsers we will write
   will be written in terms of the basic functions.

   The first useful reusable parser we'll write is one that parses
   lists of items by repeatedly trying to run a given 'Parser' until
   it fails. We call this parser 'zeroOrMore' because it attempts to
   read zero or more parses of the parser it is given from the input: -}

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p =
  do x  <- p
     xs <- zeroOrMore p
     return (x:xs)
  `orElse`
   return []

{- Some examples:

     > runParser (zeroOrMore (isChar 'a')) "aaaa"
     Just ("",[(),(),(),()])
     > runParser (zeroOrMore (isChar 'a')) "aaaabb"
     Just ("bb",[(),(),(),()])
     > runParser (zeroOrMore parseBool) "aaaabb"
     Just ("aaaabb",[])
     > runParser (zeroOrMore parseBool) "TrueFalseTrue"
     Just ("",[True,False,True])
     > runParser (zeroOrMore parseBool) "TrueFalseTrueaaaa"
     Just ("aaaa",[True,False,True])

   Note that 'zeroOrMore p' never fails: if it can't parse the input
   with 'p', then it just returns the empty list (see the third
   example above).

   'zeroOrMore p' is also greedy: it parses as far into the input as
   it can, no matter what later parses in a sequence are
   expecting. For example, the following parser uses 'zeroOrMore'
   twice to parse two lists of 'Bool'eans: -}

twoBoolLists :: Parser ([Bool],[Bool])
twoBoolLists =
  do bools1 <- zeroOrMore parseBool
     bools2 <- zeroOrMore parseBool
     return (bools1, bools2)

{- If we try this on any input, the second list will always be empty,
   because the first occurrence of 'zeroOrMore parseBool' will have
   greedily consumed all the input the second 'zeroOrMore parseBool'
   could have read:

      > runParser twoBoolLists "TrueFalseTrue"
      Just ("",([True,False,True],[]))
      > runParser twoBoolLists "TrueFalseTrueaaaa"
      Just ("aaaa",([True,False,True],[]))
      > runParser twoBoolLists "aaa"
      Just ("aaa",([],[]))
-}

{-     Part 8.6 Parsing numbers

   Parsing numbers from a decimal representation in a text file to
   actual 'Int's is a common operation for reading many kinds of
   configuration files or programming languages.

   Let's start by defining a 'Parser' for reading individual
   digits. It is useful to know that there are two helpful functions
   in the standard library's 'Data.Char' module:

     - 'isDigit :: Char -> Bool' that returns 'True' if it is given a
       digit character, and 'False' otherwise.

     - 'digitToInt :: Char -> Int' that returns the numerical
       value of a digit character.

   Putting these together with 'char' and 'failParse', we get the
   following parser that recognises individual digits and converts
   them to integers in the range 0..9: -}

parseDigit :: Parser Int
parseDigit = do c <- char
                if isDigit c then
                  return (digitToInt c)
                else
                  failParse

{- Some examples:

       > runParser parseDigit "1"
       Just ("",1)
       > runParser parseDigit "2"
       Just ("",2)
       > runParser parseDigit "x"
       Nothing
       > runParser parseDigit "A"
       Nothing
       > runParser parseDigit "123"
       Just ("23",1)
       > runParser parseDigit "3aa"
       Just ("aa",3)

  EXERCISE: Try writing 'parseDigit' without using 'isDigit' and
    'digitToInt', and instead using a 'case' to pattern match on the
    character 'c' to determine which digit (or not) it is.


  We can now use 'parseDigit' to parse positive whole numbers. Numbers
  will always have at least one digit, so we parse a single digit
  first, using 'parseDigit'. Then we use 'zeroOrMore parseDigit' to
  parse as many more digits as we can. Finally, we use a 'foldl' to
  convert the leading digit and the list of digits into an actual
  number (see the tutorial on Recursion Schemes for this use of
  foldl): -}

parsePositiveNumber :: Parser Int
parsePositiveNumber =
  do digit  <- parseDigit
     digits <- zeroOrMore parseDigit
     return (foldl (\n d -> n*10 + d) digit digits)

{- Some examples to test it:

     > runParser parsePositiveNumber "1"
     Just ("",1)
     > runParser parsePositiveNumber ""
     Nothing
     > runParser parsePositiveNumber "123"
     Just ("",123)
     > runParser parsePositiveNumber "123aaa"
     Just ("aaa",123)

  Now to complete a parser for 'Int's, we also need to deal with the
  possibility of negative numbers. Negative numbers are usually
  indicated by having a '-' sign before them. To be modular, we write
  a little parser that returns '-1' if it sees a '-' sign, and '1' if
  it doesn't: -}

parseSign :: Parser Int
parseSign = do isChar '-'
               return (-1)
            `orElse`
            do return 1

{- Now we put 'parseSign' and 'parsePositiveNumber' together to parse
   'Int's: -}

number :: Parser Int
number = do sign <- parseSign
            num  <- parsePositiveNumber
            return (sign * num)

{- Some tests:

       > runParser number "100"
       Just ("",100)
       > runParser number "1000"
       Just ("",1000)
       > runParser number "1001"
       Just ("",1001)
       > runParser number "1001aaa"
       Just ("aaa",1001)
       > runParser number "-50"
       Just ("",-50)
       > runParser number "-30"
       Just ("",-30)
       > runParser number "- 30"
       Nothing
       > runParser number "--30"
       Nothing
-}

{-    Part 8.7 : PARSING QUOTED STRINGS

   Let's do a more complex example: writing a parser for quoted string
   literals like they appear in most programming languages:

         "hello, I'm a string literal"

   String literals always end at the first '"'. To write string
   literals that represent strings containing '"'s you need to
   "escape" them by writing a '\' in front of them:

         "this is a \"string\" literal"

   So how do we do this? The first thing to do is to write a parser
   for the individual characters in a string literal (the bits between
   the '"' marks). Most characters stand for themselves, except when
   we see a bare '"', which cannot appear in a string literal, or a
   backslash '\', which indicates that the next character(s) are to be
   treated specially (e.g., "\n" for the newline character). For our
   simple parser, we'll allow a backslash followed by any character to
   stand for that character. Real string literal syntaxes allow for
   things like Unicode codepoints and so on.

   Here is the parser for individual characters, which implements the
   scheme above: -}

stringChar :: Parser Char
stringChar =
  do c <- char
     case c of
       '"'  -> failParse
       '\\' -> do c <- char
                  -- FIXME: this should be more sophisticated
                  -- see the JSON RFC
                  return c
       c    -> return c

{- Some examples (note that the '"' and '\' characters here have to be
   escaped for Haskell's own string literal syntax!):

       > runParser parseStringChar "abc"
       Just ("bc",'a')
       > runParser parseStringChar ""
       Nothing
       > runParser parseStringChar "\""
       Nothing
       > runParser parseStringChar "\\\""
       Just ("",'"')

   The last one demonstrates that by putting a backslash in front of a
   '"', we parse a '"'.

   To parse whole string literals, we first look for an opening '"',
   then zero or more string literal characters, and then a closing
   '"'. Note that 'parseStringChar' fails on '"'s, so the zeroOrMore
   parser will stop reading when it gets to the closing '"'. The code
   that implements this is as follows: -}

quotedString :: Parser String
quotedString =
  do isChar '"'
     cs <- zeroOrMore stringChar
     isChar '"'
     return cs

{- Some examples:

   1. Not a string literal:

       > runParser quotedString "hello"
       Nothing

   2. Missing a terminating '"':

       > runParser quotedString "\"hello"
       Nothing

   3. A complete string literal:

       > runParser quotedString "\"hello\""
       Just ("","hello")

   4. A string literal that terminates with more stuff after it:

       > runParser quotedString "\"hel\"lo\""
       Just ("lo\"","hel")

   5. A string literal with an escaped character:

       > runParser quotedString "\"hel\\\"lo\""
       Just ("","hel\"lo")
-}

{-     Part 8.8 PARSING JSON

   Parsing numbers and string literals is all very fine, but does the
   'Parser' approach scale to more complex things? As a case study,
   let's look at parsing of JSON (the JavaScript Object Notation), a
   commonly used data exchange format. To keep things small, our
   parser won't handle all of JSON, but a reasonable subset.

   JSON is (nearly) a subset of JavaScript syntax for describing data
   that is either:

     - a number: real JSON allows floating point numbers, but we'll
       restrict to integers for now

     - booleans: either "true" or "false"

     - nulls: the literal string "null"

     - string literals: real JSON specifies a complex system of escape
       codes for putting arbitrary unicode codepoints in strings, but
       we'll stick to the string literal syntax we defined above.

     - arrays: sequences of JSON data, separated by commas and
       surrounded by '[' and ']'

     - "objects": sequences of string keys separated from JSON data by
       a colon, all separated by commas, and surrounded by braces '{'
       and '}'.

   Here is an example JSON value:

       { "f": 1,
         "g": [ 1, null, true, "burp", false, [ "a", "aa", "aaa" ] ],
         "name": [ 4, null, 5, null, { "a" : 10 } ]
       }

   JSON values can be arbitrarily nested as deep as you like. The
   official syntax description is in the IETF RFC 7159:

     https://tools.ietf.org/html/rfc7159

     (IETF is the "Internet Engineering Task Force", RFC is "Request
      for Comments", which is how internet standards are usually
      distibuted.)


   The first thing to do is to define a Haskell datatype to represent
   JSON values. The parser we will write will generate values in this
   datatype. Following the description given above, we use the
   following representation, with a constructor for each kind of thing
   that a JSON might be: -}

data JSON
  = Number  Int
  | Boolean Bool
  | String  String
  | Null
  | Array   [JSON]
  | Object  [(String,JSON)]
  deriving Show

{- For objects, we just store them as a list of key-value pairs. The
   JSON standard doesn't specify what happens if a key is repeated, or
   if JSON objects with the same key/values in a different order are
   to be treated as the same, so we will just store the keys in the
   order they are written.

   To write a parser for JSON values, we'll break it down into parsers
   for each of the individual kinds of JSON values.

   We already have parsers for 'Int's and string literals.

   Last lecture, we defined a parser for 'Bool's, but that was in
   Haskell syntax ("True" and "False"), not JavaScript syntax ("true"
   and "false"). So we define another parser for JSON booleans: -}

parseJSONBool :: Parser Bool
parseJSONBool =
  do isString "true"
     return True
  `orElse`
  do isString "false"
     return False

{- The other simple (in the sense of not being made of multiple bits)
   JSON value is "null". A parser for "null"s: -}

parseNull :: Parser ()
parseNull =
  do isString "null"
     return ()

{- The remaining kinds of JSON value to parse are arrays:

     [ 1, 2, null, false, "blarp" ]

   and objects:

     { "Aberdeen": 5, "Celtic": 0 }

   We could just launch in and write parsers for these two
   directly. But there is a bit of common structure that we can pull
   out. First, they both consist of sequences of things separated by
   commas. Second, they both allow whitespace to appear arbitrarily
   between elements.

   Let's handle the second one first. There are multiple whitespace
   characters in Unicode (spaces, tabs, newlines, non-breaking spaces,
   zero-width spaces, etc.), so we rely on the standard library
   function 'isSpace' to detect them for us: -}

parseSpace :: Parser ()
parseSpace =
  do c <- char
     if isSpace c then return () else failParse

{- Arbitrary whitespace is zero or spaces: -}

parseWhitespace :: Parser ()
parseWhitespace =
  do zeroOrMore parseSpace
     return ()

{- Some tests:

      > runParser parseWhitespace "       stuff"
      Just ("stuff",())

      > runParser parseWhitespace "stuff"
      Just ("stuff",())

   Returning to the first point: items separated by commas. The
   'zeroOrMore' parser enabled us to parse zero or more repetitions. A
   common pattern is to have zero or more repetitions of items,
   separated by a character such as a comma or a semicolon. Can we use
   'zeroOrMore to build a parser for such repetitions?

   Here's one way of doing it. The function 'sepBy' takes two
   arguments: the first is a parser for the separator, and the second
   is a parser for the items. It returns a list of parsed items.

   It works by first trying to parse a single item (using 'p') and
   then zero or more repetitions of the 'separator' followed by a
   'p'. (The semicolon syntax allows us to write a two line "do" block
   on a single line.) If that parser fails, we fall back to just
   returning the empty list. -}

sepBy :: Parser () -> Parser a -> Parser [a]
sepBy separator p =
  do x <- p
     xs <- zeroOrMore (do separator; p)
     return (x:xs)
  `orElse`
  return []

{- To test it, here's a parser for commas: -}

comma :: Parser ()
comma = isChar ','

{- Let's try some examples, 'Int's separated by commas:

   1. Some items:

         > runParser (sepBy comma number) "1,2,3,4"
         Just ("",[1,2,3,4])

   2. No items:

         > runParser (sepBy comma number) ""
         Just ("",[])

   3. Some items with trailing stuff:

         > runParser (sepBy comma number) "1,2,3,4xyz"
         Just ("xyz",[1,2,3,4])

   So far as expected, but JSON syntax also allows for spaces, which
   our parser doesn't:

         > runParser (sepBy comma number) "1 , 2, 3, 4"
         Just (" , 2, 3, 4",[1])

   It recognised the '1', but the space between that and the comma
   caused it to stop consuming characters, and it returned the rest as
   left over input.

   How to fix this? We could modify 'sepBy' to allow extra whitespace,
   but this would prevent us from using it in situations where extra
   whitespace isn't allowed. Another way to fix it is to alter the
   separator parser to allow whitespace before and after the comma: -}

spacedComma :: Parser ()
spacedComma =
  do parseWhitespace
     isChar ','
     parseWhitespace

{- Using 'spacedComma' as a separator, we get the expected behaviour:

      > runParser (sepBy spacedComma number) "1 , 2, 3, 4"
      Just ("",[1,2,3,4])

   JSON arrays are comma separated lists of values that are also
   surrounded by square brackets: '[' and ']'. Writing a parser for
   lists in this form is a matter of writing a small wrapper around
   'spacedComma' separated items. We leave the parser to be used for
   parsing the items as a parameter 'p' for now -- we will use the
   parser for JSON elements when we write it below. -}

parseList :: Parser a -> Parser [a]
parseList p =
  do isChar '['
     parseWhitespace
     xs <- sepBy spacedComma p
     parseWhitespace
     isChar ']'
     return xs

{- Some examples, for testing:

   1. Parsing a list of boolean:

        > runParser (parseList number) "[ 1, 2, 3, 4 ]"
        Just ("",[1,2,3,4])

   2. Trailing commas are rejected:

        > runParser (parseList number) "[ 1, 2, 3, 4,  ]"
        Nothing

   3. Empty lists:

        > runParser (parseList number) "[ ]"
        Just ("",[])

   4. Lists without spaces:

        > runParser (parseList number) "[1,2,3,4]"
        Just ("",[1,2,3,4])

   5. List with one item:

        > runParser (parseList number) "[1]"
        Just ("",[1])

   6. Parsing a list of 'Int's, but giving it a boolean:

        > runParser (parseList number) "[true]"
        Nothing

   7. Parsing lists of booleans:

        > runParser (parseList parseJSONBool) "[true]"
        Just ("",[True])
        > runParser (parseList parseJSONBool) "[true, false]"
        Just ("",[True,False])


   Now that we've done JSON arrays, we also want to parse JSON
   objects. Recall that a JSON object looks like:

         { "Aberdeen": 5, "Celtic": 0 }

   Like JSON arrays, they are comma separated lists of items,
   surrounded by brackets '{' and '}'. This time, each item consists
   of a pair of a string literal for the field name, a colon, and a
   value. Let's write a parser for object items that takes the parser
   to use for the value as an argument: -}

parseObjectItem :: Parser a -> Parser (String, a)
parseObjectItem p =
  do fieldname <- quotedString
     parseWhitespace
     isChar ':'
     parseWhitespace
     value <- p
     return (fieldname, value)

{- Some tests:

   1. Successful object items:

         > runParser (parseObjectItem number) "\"Aberdeen\" : 5"
         Just ("",("Aberdeen",5))
         > runParser (parseObjectItem number) "\"Celtic\" : 0"
         Just ("",("Celtic",0))

   2. Fieldnames must have quotes:

         > runParser (parseObjectItem number) "Celtic : 0"
         Nothing

   3. The colon is required:

         > runParser (parseObjectItem number) "\"Celtic\" 0"
         Nothing

   4. But we don't need all the whitespace:

         > runParser (parseObjectItem number) "\"Aberdeen\":5"
         Just ("",("Aberdeen",5))

   5. We can change what the parser used for the value is:

         > runParser (parseObjectItem parseJSONBool) "\"thisBooleanIs\":false"
         Just ("",("thisBooleanIs",False))

   Now to parse JSON objects, we write the same wrapper as for JSON
   arrays, except with a different parser for the items, and different
   kinds of brackets: -}

parseObject :: Parser a -> Parser [(String,a)]
parseObject p =
  do isString "{"
     parseWhitespace
     xs <- sepBy spacedComma (parseObjectItem p)
     parseWhitespace
     isString "}"
     return xs

{- A quick test:

     > runParser (parseObject number) "{ \"Aberdeen\": 5, \"Celtic\" : 0}"
     Just ("",[("Aberdeen",5),("Celtic",0)])


   Now we are ready to write a parser for JSON values. Remember that a
   JSON value is either: a number (an 'Int' here), a string literal, a
   boolean, null, an array of JSON values, or a JSON object. We follow
   this specification directly, writing a parser that tries each in
   turn, falling through to the next case on failure using 'orElse': -}

parseJSON :: Parser JSON
parseJSON =
  do num <- number
     return (Number num)
  `orElse`
  do s <- quotedString
     return (String s)
  `orElse`
  do b <- parseJSONBool
     return (Boolean b)
  `orElse`
  do parseNull
     return Null
  `orElse`
  do items <- parseList parseJSON
     return (Array items)
  `orElse`
  do fields <- parseObject parseJSON
     return (Object fields)

{- Some JSON test data, as a string (the backslashes at the end and
   start of each line are how we can write long strings in Haskell
   without having newlines in them): -}

testJSONString :: String
testJSONString =
  "{ \"f\": 1,\
   \  \"g\": [ 1, null, true, \"burp\", false, \
   \           [ \"a\", \"aa\", \"aaa\" ] ],\
   \  \"name\": [ 4, null, 5, null, { \"a\" : 1 } ]\
   \}"

{- Let's run it through the parser we just wrote (formatted for
   readability):

   > runParser parseJSON testJSONString
   Just ("",Object [("f",Number 1),
                    ("g",Array [Number 1,
                                Null,
                                Boolean True,
                                String "burp",
                                Boolean False,
                                Array [String "a",
                                       String "aa",
                                       String "aaa"]]),
                    ("name",Array [Number 4,
                                   Null,
                                   Number 5,
                                   Null,
                                   Object [("a",Number 1)]])])

   Using the IO monad's 'readFile' function, we can now write a
   function that reads JSON data from a file. This parser is very
   strict: it does not allow any leading whitespace or and always
   requires a newline character after the JSON value. Can you change
   it so it is more liberal? -}

readJSON :: FilePath -> IO (Maybe JSON)
readJSON path =
  do fileContents <- readFile path
     case runParser parseJSON fileContents of
       Just ("\n", json) -> return (Just json)
       Nothing           -> return Nothing
       Just _            -> return Nothing

{------------------------------------------------------------------------------}
{- Appendix                                                                   -}

{- The following two type class instance definitions are required by the
   way that the Monad type class is defined in standard Haskell. We'll
   come back to why in Week 09. -}

instance Applicative Parser where
  pure      = return
  mf <*> ma = do f <- mf; a <- ma; return (f a)

instance Functor Parser where
  fmap f p = pure f <*> p

{------------------------------------------------------------------------------}
{- TUTORIAL QUESTIONS                                                         -}
{------------------------------------------------------------------------------}

{- 2. Write a function 'withOutputFile' that does what 'withInputFile'
   does but for output. Use it to write an exception safe version of
   'writeToFile'. -}

{- 3. Rewrite 'isString' using 'for_'. -}

{- 4. For the 'Monad' instance for 'Parser', I said that it is a
      combination of 'Maybe' (for simulating exceptions), and 'State'
      (for simulating mutable state). So you could also do parsers in
      a language like Java that has built-in state and
      exceptions. (Try it!  use an instance field to keep the current
      input in.) What is different about the 'orElse' function? What
      facility would you need in Java to make it work? -}
