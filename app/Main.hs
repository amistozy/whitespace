module Main where

import Solution (whitespace)
import System.Environment (getArgs)

main :: IO ()
main = do
  (file : _) <- getArgs
  code <- readFile file
  input <- getContents
  let result = whitespace code input
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right output -> putStrLn output
