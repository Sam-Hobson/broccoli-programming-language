module ExprLexer
where

import EquationLexer (Equation, equation)
import Parser
import BasicParserFuncs
import SyntaxParserFuncs

data Expr =
    Constant Integer        |
    Equation Equation       |
    String String           |
    Symbol String           |
    SymbolCall String Expr  |
    None
    deriving (Show)

symbolCall :: Parser Expr
symbolCall = do
    name <- idToken
    content <- surround "(" (expr ||| pure None) ")"
    pure $ SymbolCall name content


expr :: Parser Expr
expr = (Equation <$> tok equation)      |||
       (String <$> tok innerString)     |||
       tok symbolCall  |||
       (Symbol <$> tok idToken)
