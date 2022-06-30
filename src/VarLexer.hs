module VarLexer where

import Parser
import BasicParserFuncs
import SyntaxParserFuncs
import ExprLexer

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

-- Parses a full variable.
--
-- >>> parse varParser "num1: int = 2;"
-- <Parsed: Var {tokenId = "num1", dataType = PInt, value = Equation (Number 2)}> <Remaining: "">
--
-- >>> isError (parse varParser "num1: int = hi;")
-- True
--
-- >>> parse varParser "num1: string = \"salad\";"
-- <Parsed: Var {tokenId = "num1", dataType = PString, value = String "salad"}> <Remaining: "">
varParser :: Parser Var
varParser = do
  spaces
  a <- idToken
  b <- typeToken
  c <- assignmentToken
  endLine
  pure $ Var a b c
