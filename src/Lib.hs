module Lib where

import Text.ParserCombinators.Parsec
    ( parse, digit, many1, between, char, ParseError, Parser, spaces, skipMany, skipMany1 )
import Text.Parsec ( try, many, (<|>) )
import Control.Applicative ()
import Text.Parsec.Combinator (option)
import Control.Monad (liftM2)

-- + (123)
-- + (+ 1 2)
-- - (+ (1) (2))
-- + (+ (+ 1 2) 3)

newtype Value = Value Int deriving (Show)
data Op = Plus | Minus deriving (Show)
data SExpr = SValue Value | SOp Op SExpr SExpr deriving (Show)

openBracket :: Parser Char
openBracket = char '('
closeBracket :: Parser Char
closeBracket = char ')'

value :: Parser Value
value = do
  n <- many1 digit
  return (Value $ read n)

op :: Parser Op
op =  do
  a <- char '+' <|> char '-'
  case a of
     '+' -> return Plus
     '-' -> return Minus
     _ -> return Minus

svalue :: Parser SExpr
svalue = SValue <$> (try (between openBracket closeBracket inner) <|> inner)
  where
    inner = spaces *> value <* spaces

sop :: Parser SExpr
sop = between openBracket closeBracket $ do
  o <- spaces *> op <* spaces
  e1 <- sexpr <* spaces
  e2 <- sexpr <* spaces
  return $ SOp o e1 e2
-- sop = do
--   openBracket
--   o <- op
--   spaces
--   e1 <- sexpr
--   spaces
--   e2 <- sexpr
--   closeBracket
--   return $ SOp o e1 e2

sexpr :: Parser SExpr
sexpr = try svalue <|> sop

eval :: SExpr -> Int
eval e = case e of
  SValue (Value v) -> v
  SOp op e1 e2 -> case op of
    Plus -> (+) (eval e1) (eval e2)
    Minus -> (-) (eval e1) (eval e2)


parse_and_eval :: Parser SExpr -> String -> Either ParseError Int
parse_and_eval p s = case parse p "" s of
  Left err -> Left err
  Right v -> Right $ eval v
