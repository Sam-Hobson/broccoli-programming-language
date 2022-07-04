module ExprLexer where

import BasicParserFuncs
import ParseTypes (Expr (..), FunctionData, Equation(..))
import Parser
import SyntaxParserFuncs (idToken)

op :: Char -> Parser Char -- parse a single char operator
op c = do
  is c
  whitespace
  pure c

times :: Parser (Equation -> Equation -> Equation)
times = op '*' >> pure Times

add :: Parser (Equation -> Equation -> Equation)
add = (op '+' >> pure Plus) ||| (op '-' >> pure Minus)

equation :: Parser Equation
equation = do
    r <- chain term add
    case r of
        E e -> failed $ UnexpectedString (show e)
        _ -> pure r

term :: Parser Equation
term = chain atomicEq times

atomicEq :: Parser Equation
atomicEq =
    (E . Number <$> tok integer)            |||
    (E . String <$> tok innerString)        |||
    (E . SymbolCall <$> tok functionCall)   |||
    (E . Symbol <$> idToken)

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

functionCall :: Parser FunctionData
functionCall = do
  name <- idToken
  content <- surround "(" (sepby (expr ||| pure None) (is ',')) ")"
  pure (name, content)

expr :: Parser Expr
expr =
  (Equation <$> tok equation)
    ||| (Number <$> tok integer)
    ||| (String <$> tok innerString)
    ||| (SymbolCall <$> tok functionCall)
    ||| (Symbol <$> tok idToken)
