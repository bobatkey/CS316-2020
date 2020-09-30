module Main where

main :: IO ()
main = do
  contents <- readFile "README.md"
  let output = unlines (reverse (lines contents))
  writeFile "test.txt" output
  putStrLn "hello CS316!"
