{-# Language NamedFieldPuns #-}
module Lib where

import Text.ParserCombinators.Parsec
  ( parse, digit, many1, between, char, ParseError, Parser, spaces, skipMany, skipMany1, sepBy, oneOf, string, anyChar, alphaNum )
import Text.Parsec
  ( try, many, (<|>), option)
import Text.Read (readMaybe, readEither)
import Control.Monad.Trans.State
import Control.Monad.IO.Class
import Data.Map as M (Map(..), insert, empty, toList)
import System.IO
import Control.Monad (forM,forM_)

data Value = ValueI Int
           | ValueD Double
    deriving (Show, Eq)
data Op = Plus
        | Minus
        | Multiply
    deriving (Show)
data Def = DefVar String Value
  deriving (Show)
data SExpr = SValue Value
           | SOp Op [SExpr]
           | SDef Def
    deriving (Show)
newtype World = World {variables :: M.Map String Value}
instance Show World where
  show World {variables} = 
    let
    a = mconcat $ fmap (\(name, val) -> '\t':name ++ ": " ++ show val ++ "\n") (M.toList variables)
    in
      "World:\n" ++ a
addToWorld :: World -> String -> Value -> World
addToWorld World {variables} name value = World $ M.insert name value variables

openBracket :: Parser Char
openBracket = char '('
closeBracket :: Parser Char
closeBracket = char ')'

digitOrDot :: Parser Char 
digitOrDot = digit <|> char '.'

value :: Parser Value
value = do
  n <- many1 digitOrDot
  case readMaybe n of
    Just v -> pure $ ValueI v
    Nothing -> case readEither n of
      Left err -> error err
      Right vv -> pure $ ValueD vv

op :: Parser Op
op =  do
  a <- oneOf ['+', '-', '*']
  case a of
     '+' -> pure Plus
     '-' -> pure Minus
     '*' -> pure Multiply
     _ -> error "no such operator"

svalue :: Parser SExpr
svalue = SValue <$> (try (between openBracket closeBracket inner) <|> inner)
  where
    inner = spaces *> value <* spaces

sop :: Parser SExpr
sop = between openBracket closeBracket $ do
  o <- spaces *> op <* spaces
  es <- sepBy sexpr spaces
  pure $ SOp o es

sdefvar :: Parser SExpr
sdefvar = between openBracket closeBracket $ do
  _ <- string "defvar" <* spaces
  name <- do
    firstSym <- anyChar
    tailSyms <- many1 alphaNum
    pure $ firstSym:tailSyms
  val <- spaces *> value
  pure $ SDef $ DefVar name val

sexpr :: Parser SExpr
sexpr = try svalue <|> try sop <|> sdefvar

evalWithState :: SExpr -> StateT World IO ()
evalWithState sexpr = do
  case sexpr of
    SValue (ValueI v) -> liftIO $ print v
    SValue (ValueD v) -> liftIO $ print v
    SOp op sexpr -> undefined
    SDef (DefVar name value) -> do
      w <- get
      let new_w = addToWorld w name value
      liftIO $ print new_w
      put new_w
      pure ()

parseAndEvalWithStateLoop :: World -> IO ()
parseAndEvalWithStateLoop w = do
  putStr ">> "
  hFlush stdout
  l <- getLine
  new_w <- case parse sexpr "" l of
        Left err -> error $ show err
        Right v -> execStateT (evalWithState v) w
  parseAndEvalWithStateLoop new_w

run :: IO ()
run = parseAndEvalWithStateLoop (World M.empty)
