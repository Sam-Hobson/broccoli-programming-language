module VarLexer where

import BasicParserFuncs
import ExprLexer
import Parser
import SyntaxParserFuncs

data Var = Var {tokenId :: String, dataType :: Type, value :: Expr}
  deriving (Show)

-- Parses the assignment. This is everything including and beyond
-- the equals sign.
--
-- >>> parse assignmentToken "= 1"
-- <Parsed: Equation (Number 1)> <Remaining: "">
--
-- >>> parse assignmentToken "= 1+2*3"
-- <Parsed: Equation (Plus (Number 1) (Times (Number 2) (Number 3)))> <Remaining: "">
assignmentToken :: Parser Expr
assignmentToken =
  tok (is '=') >> expr

-- Parses the declaration of a variable.
--
-- >>> parse varDeclaration "x: int"
-- <Parsed: Var {tokenId = "x", dataType = PInt, value = None}> <Remaining: "">
varDeclaration :: Parser Var
varDeclaration = do
  spaces
  a <- idToken
  b <- typeToken
  pure $ Var a b None

-- Parses a full variable.
--
-- >>> parse varInitialisation "num1: int = 2"
-- <Parsed: Var {tokenId = "num1", dataType = PInt, value = Equation (Number 2)}> <Remaining: "">
--
-- >>> isError (parse varInitialisation "num1: int = hi")
-- True
--
-- >>> parse varInitialisation "num1: string = \"salad\";"
-- <Parsed: Var {tokenId = "num1", dataType = PString, value = String "salad"}> <Remaining: "">
varInitialisation :: Parser Var
varInitialisation = do
  v <- varDeclaration
  a <- assignmentToken
  pure $ v {value = a}
