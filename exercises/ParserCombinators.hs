{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}
module ParserCombinators where

import           Result
import           Data.Char (isSpace, isLower, isUpper, isNumber,
                            digitToInt, isAlpha, isAlphaNum)

{- This is the code for the parser combinators you should use to
   implement your parsers. You may want to consult this code to help
   you write your parser, but do not modify it. -}

newtype Parser a = MkParser (String -> Result (a, String))

runParser :: Parser a -> String -> Result (a, String)
runParser (MkParser f) = f

parseWith :: Parser a -> String -> Result a
parseWith (MkParser f) s =
  case f s of
    Error msg  -> Error msg
    Ok (a, "") -> Ok a
    Ok (a, s)  -> Error ("Trailing input: " ++ show s)

instance Monad Parser where
  return x = MkParser (\s -> Ok (x,s))

  p >>= k =
    MkParser (\s -> case runParser p s of
                      Error msg  -> Error msg
                      Ok (a, s') -> runParser (k a) s')

instance Functor Parser where
  fmap f p =
    do x <- p
       return (f x)

instance Applicative Parser where
  pure = return
  pf <*> pa = do f <- pf; a <- pa; return (f a)

orElse :: Parser a -> Parser a -> Parser a
orElse p1 p2 =
  MkParser (\input ->
              case runParser p1 input of
                Ok (a,input1) -> Ok (a,input1)
                Error _       -> runParser p2 input)

failParse :: String -> Parser a
failParse msg = MkParser (\s -> Error msg)

char :: Parser Char
char =
  MkParser
  (\input ->
      case input of
        []     -> Error "unexpected end of input was found"
        (c:cs) -> Ok (c, cs))

isChar :: Char -> Parser ()
isChar expected =
  do seen <- char
     if expected == seen then
       return ()
     else
       failParse ("Expecting " ++ show expected ++ ", got " ++ show seen)

satisfies :: String -> (Char -> Bool) -> Parser Char
satisfies p_description p = do
  c <- char
  if p c then return c
    else failParse ("Expecting " ++ p_description ++ ", got " ++ show c)

isString :: String -> Parser ()
isString expected =
  mapM_ isChar expected
  `orElse`
  failParse ("Expecting '" ++ expected ++ "'")

quotedStringChar :: Parser Char
quotedStringChar =
  do c <- char
     case c of
       '"'  -> failParse ""
       '\\' -> char
       c    -> return c

quotedString :: Parser String
quotedString =
  do isChar '"'
     cs <- zeroOrMore quotedStringChar
     isChar '"'
     return cs

digit :: Parser Int
digit = do
  c <- char
  if isNumber c then
    return (digitToInt c)
  else
    failParse "Expecting a digit"

oneOrMore :: Parser a -> Parser [a]
oneOrMore p =
  do x  <- p
     xs <- zeroOrMore p
     return (x:xs)

zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p =
  do x <- p
     xs <- zeroOrMore p
     return (x:xs)
  `orElse`
  return []

number :: Parser Int
number =
  foldl (\l r -> l*10+r) 0 <$> oneOrMore digit
  `orElse`
  failParse "Expecting a positive number"

space :: Parser ()
space = do satisfies "a space character" (\c -> c == ' ')
           return ()

spaces :: Parser ()
spaces = do zeroOrMore space
            return ()

newline :: Parser ()
newline = isChar '\n' `orElse` isString "\r\n"

identifier :: Parser String
identifier =
  do c  <- satisfies "alphabetic character" isAlpha
     cs <- zeroOrMore (satisfies "alphanumeric character" isAlphaNum)
     return (c:cs)
  `orElse`
  failParse "Expecting an identifier"

sepBy :: Parser () -> Parser a -> Parser [a]
sepBy sep p =
  do x  <- p
     xs <- zeroOrMore (do sep; p)
     return (x:xs)
  `orElse`
  return []
