module ExprLexer where

import BasicParserFuncs
import ParseTypes
import Parser
import SyntaxParserFuncs

-- BOOLEAN OPERATIONS

eqOP :: Parser (BoolOp -> BoolOp -> BoolOp)
eqOP = tok (string "==") >> pure EqOP

greaterOP :: Parser (BoolOp -> BoolOp -> BoolOp)
greaterOP = tok (string ">") >> pure GreaterOP

greaterEqOP :: Parser (BoolOp -> BoolOp -> BoolOp)
greaterEqOP = tok (string ">=") >> pure GreaterEqOP

lessOP :: Parser (BoolOp -> BoolOp -> BoolOp)
lessOP = tok (string "<") >> pure LessOP

lessEqOP :: Parser (BoolOp -> BoolOp -> BoolOp)
lessEqOP = tok (string "<=") >> pure LessEqOP

notEqOP :: Parser (BoolOp -> BoolOp -> BoolOp)
notEqOP = tok (string "!=") >> pure NotEqOP

andOP :: Parser (BoolOp -> BoolOp -> BoolOp)
andOP = tok (string "&&" ||| string "and") >> pure AndOP

orOP :: Parser (BoolOp -> BoolOp -> BoolOp)
orOP = tok (string "||" ||| string "or") >> pure OrOP

notOP :: Parser (BoolOp -> BoolOp)
notOP = tok (string "not" ||| string "!") >> pure NotOP

e1 :: Parser BoolOp
e1 = atomicOp E1

boolOp :: Parser BoolOp
boolOp = do
  let ops = [eqOP, greaterOP, greaterEqOP, lessOP, lessEqOP, notEqOP, andOP, orOP]
  r <- foldl chain ((notOP <*> e1) ||| e1) ops
  case r of
    E1 e -> failed $ UnexpectedString (show e)
    _ -> pure r

-- EQUATION OPERATIONS

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
  r <- foldl chain (atomicOp E) [times, add]
  case r of
    E e -> failed $ UnexpectedString (show e)
    _ -> pure r

atomicOp :: (Expr -> a) -> Parser a
atomicOp f =
  (f . Number <$> tok integer)
    ||| (f . String <$> tok innerString)
    ||| (f . SymbolCall <$> tok functionCall)
    ||| (f . Symbol <$> idToken)
    ||| (f . Priority <$> priority)

-- EXPRESSION OPERATIONS

priority :: Parser Expr
priority = surround "(" expr ")"

functionCall :: Parser FunctionData
functionCall = do
  name <- idToken
  content <- surround "(" (sepby (expr ||| pure None) (tok $ is ',')) ")"
  pure (name, content)

expr :: Parser Expr
expr =
    (Priority <$> priority)
    ||| (Equation <$> tok equation)
    ||| (BoolOp <$> tok boolOp)
    ||| (Number <$> tok integer)
    ||| (String <$> tok innerString)
    ||| (SymbolCall <$> tok functionCall)
    ||| (Symbol <$> tok idToken)
