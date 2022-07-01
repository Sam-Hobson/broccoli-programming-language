module EquationLexer where

import Parser
import BasicParserFuncs
import SyntaxParserFuncs (idToken)
import ParseTypes ( Equation(..) )

number :: Parser Equation
number = whitespace >> Number <$> integer

symbol :: Parser Equation
symbol = Symbol1 <$> idToken

op :: Char -> Parser Char -- parse a single char operator
op c = do
  whitespace
  is c
  pure c

times :: Parser (Equation -> Equation -> Equation)
times = op '*' >> pure Times

add :: Parser (Equation -> Equation -> Equation)
add = (op '+' >> pure Plus) ||| (op '-' >> pure Minus)

equation :: Parser Equation
equation = do
    res <- chain term add
    case res of
        Number n -> failed $ UnexpectedString (show n)
        _ -> pure res

term :: Parser Equation
term = chain atomicEq times

atomicEq :: Parser Equation
atomicEq = number ||| symbol

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
