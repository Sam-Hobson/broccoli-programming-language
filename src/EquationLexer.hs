module EquationLexer where

import Parser
import ParserFuncs

data Equation
  = Plus Equation Equation
  | Minus Equation Equation
  | Times Equation Equation
  | Number Integer
  deriving (Show)

number :: Parser Equation
number = spaces2 >> Number <$> integer

op :: Char -> Parser Char -- parse a single char operator
op c = do
  spaces2
  is c
  pure c

times :: Parser (Equation -> Equation -> Equation)
times = op '*' >> pure Times

add :: Parser (Equation -> Equation -> Equation)
add = (op '+' >> pure Plus) ||| (op '-' >> pure Minus)

equation :: Parser Equation
equation = chain term add

term :: Parser Equation
term = chain number times

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest
  where
    rest a =
      ( do
          f <- op
          b <- p
          rest (f a b)
      )
        ||| pure a
