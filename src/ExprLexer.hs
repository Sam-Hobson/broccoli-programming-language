module ExprLexer where

import BasicParserFuncs
import EquationLexer (equation)
import ParseTypes (Expr (..), Code(..), FunctionData)
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
    ||| (String <$> tok innerString)
    ||| (SymbolCall <$> tok functionCall)
    ||| (Symbol <$> tok idToken)
