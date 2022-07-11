module ExprLexer where

import BasicParserFuncs
import ParseTypes
import Parser
import SyntaxParserFuncs

-- BOOLEAN OPERATIONS

eqOP :: Parser (BoolCompOp -> BoolCompOp -> BoolCompOp)
eqOP = tok (string "==") >> pure EqOP

greaterOP :: Parser (BoolCompOp -> BoolCompOp -> BoolCompOp)
greaterOP = tok (string ">") >> pure GreaterOP

greaterEqOP :: Parser (BoolCompOp -> BoolCompOp -> BoolCompOp)
greaterEqOP = tok (string ">=") >> pure GreaterEqOP

lessOP :: Parser (BoolCompOp -> BoolCompOp -> BoolCompOp)
lessOP = tok (string "<") >> pure LessOP

lessEqOP :: Parser (BoolCompOp -> BoolCompOp -> BoolCompOp)
lessEqOP = tok (string "<=") >> pure LessEqOP

notEqOP :: Parser (BoolCompOp -> BoolCompOp -> BoolCompOp)
notEqOP = tok (string "!=") >> pure NotEqOP

andOP :: Parser (BoolLogicalOp -> BoolLogicalOp -> BoolLogicalOp)
andOP = tok (string "&&" ||| string "and") >> pure AndOP

orOP :: Parser (BoolLogicalOp -> BoolLogicalOp -> BoolLogicalOp)
orOP = tok (string "||" ||| string "or") >> pure OrOP

notOP :: Parser (BoolLogicalOp -> BoolLogicalOp)
notOP = tok (string "not" ||| string "!") >> pure NotOP

e1 :: Parser BoolCompOp
e1 = atomicOp E1

e2 :: Parser BoolLogicalOp
e2 = atomicOp E2

boolCompOp :: Parser BoolCompOp
boolCompOp = do
  r <- foldl chain e1 [eqOP, greaterOP, greaterEqOP, lessOP, lessEqOP, notEqOP]
  case r of
    E1 e -> failed $ UnexpectedString (show e)
    _ -> pure r

boolLogicalOp :: Parser BoolLogicalOp
boolLogicalOp = do
  r <- foldl chain ((notOP <*> e2) ||| e2) [andOP, orOP]
  case r of
    E2 e -> failed $ UnexpectedString (show e)
    _ -> pure r

equation :: Parser Equation
equation = do
  r <- foldl chain (atomicOp E) [times, add]
  case r of
    E e -> failed $ UnexpectedString (show e)
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

atomicOp :: (Expr -> a) -> Parser a
atomicOp f =
  (f . Number <$> tok integer)
    ||| (f . String <$> tok innerString)
    ||| (f . Boolean <$> tok boolean)
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
  (Equation <$> tok equation)
    ||| (BoolCompOp <$> tok boolCompOp)
    ||| (BoolLogicalOp <$> tok boolLogicalOp)
    ||| (Priority <$> priority)
    ||| (Number <$> tok integer)
    ||| (Boolean <$> tok boolean)
    ||| (String <$> tok innerString)
    ||| (SymbolCall <$> tok functionCall)
    ||| (Symbol <$> tok idToken)
