module Main where

import Text.Parsec (parse)
import Lib

main :: IO ()
main = do
  l <- getLine
  putStrLn $ case parse sexpr "" l of
    Right e -> show $ eval e
    Left err -> show err
