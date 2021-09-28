module Lib where

import Text.ParserCombinators.Parsec
import Text.Parsec
import Control.Applicative

-- + (123)
-- - (+ 1 2)
-- + (+ (1) (2))
-- - (+ (+ 1 2) 3)
newtype Value = Value Int deriving (Show)
data Op = Plus | Minus deriving (Show)
data SExpr = SValue Value | SOp Op SExpr SExpr deriving (Show)

openBracket :: Parsec String st Char
openBracket = char '('
closeBracket :: Parsec String st Char
closeBracket = char ')'
brackets = between openBracket closeBracket

value :: Parsec String st Value
value = do
  n <- many1 digit
  return (Value $ read n)

op :: Parsec String st Op
op =  do
  a <- char '+' Text.Parsec.<|> char '-'
  case a of
     '+' -> return Plus
     '-' -> return Minus
     _ -> return Minus

svalue :: Parsec String st SExpr
svalue = do
  -- openBracket
  v <- value
  -- closeBracket
  return $ SValue v

sop :: Parsec String st SExpr
sop = do
  openBracket
  o <- op
  Text.Parsec.many $ char ' '
  e1 <- sexpr
  Text.Parsec.many $ char ' '
  e2 <- sexpr
  closeBracket
  return $ SOp o e1 e2

sexpr :: Parsec String st SExpr
sexpr = Text.Parsec.try svalue Text.Parsec.<|> sop

eval :: SExpr -> Int
eval e = case e of
  SValue (Value v) -> v
  SOp op e1 e2 -> case op of
    Plus -> (+) (eval e1) (eval e2)
    Minus -> (-) (eval e1) (eval e2)
  
