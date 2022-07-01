module ExprLexer where

import BasicParserFuncs
import EquationLexer (equation)
import ParseTypes (Expr (..), FunctionData)
import Parser
import SyntaxParserFuncs (idToken)

functionCall :: Parser FunctionData
functionCall = do
  name <- idToken
  content <- surround "(" (expr ||| pure None) ")"
  pure (name, content)

expr :: Parser Expr
expr =
  (Equation <$> tok equation)
    ||| (Constant <$> tok integer)
    ||| (String <$> tok innerString)
    ||| (SymbolCall <$> tok functionCall)
    ||| (Symbol <$> tok idToken)
