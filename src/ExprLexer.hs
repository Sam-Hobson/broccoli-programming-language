{-# LANGUAGE InstanceSigs #-}

module ExprLexer 
where

import Parser
import ParserFuncs

data Expr = Plus Expr Expr
          | Minus Expr Expr
          | Times Expr Expr
          | Number Integer
  deriving Show

number :: Parser Expr  
number = spaces2 >> Number . read . (:[]) <$> digit

op :: Char -> Parser Char -- parse a single char operator
op c = do
   spaces2
   is c
   pure c

times :: Parser (Expr -> Expr -> Expr)
times = op '*' >> pure Times


add :: Parser (Expr -> Expr -> Expr)
add = (op '+' >> pure Plus) ||| (op '-' >> pure Minus)

expr :: Parser Expr
expr = chain term add

term :: Parser Expr
term = chain number times

chain :: Parser a -> Parser (a->a->a) -> Parser a
chain p op = p >>= rest
   where
   rest a = (do
               f <- op
               b <- p
               rest (f a b)
            ) ||| pure a
