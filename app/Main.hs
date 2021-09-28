module Main where

import Lib

main :: IO ()
main = do
  l <- getLine
  putStrLn $ case parse_and_eval sexpr l of
    Right v -> show v
    Left err -> show err
