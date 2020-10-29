module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import Control.Monad (filterM)
import Result
import ParserCombinators
import Ex3

{- 3.3.0

   Edit to this to implement a command line tool for filtering
   CSV files: -}

liftResult :: Result a -> IO a
liftResult (Ok a) = return a
liftResult (Error msg) = do putStrLn ("ERROR: " ++ msg)
                            exitFailure

main :: IO ()
main =
  do args <- getArgs
     liftResult (Error ("Arguments are: " ++ concat (intersperse "," args)))
