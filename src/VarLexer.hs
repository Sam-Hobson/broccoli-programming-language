module VarLexer where

import EquationLexer hiding (Number)
import Parser
import ParserFuncs

-- Data types that can be parsed
data Type
  = PInt
  | PString
  deriving (Show)

data Assignment
  = Equation Equation
  | Number Integer
  | String String
  deriving (Show)

data Var = Var {tokenId :: String, dataType :: Type, value :: Assignment}
  deriving (Show)

-- Parses the next word/id. This word can contain
-- numbers but cannot start with one.
--
-- >>> parse idToken "var1: int = 3"
-- <Parsed: "var1"> <Remaining: ": int = 3;">
idToken :: Parser String
idToken = do
  c <- alpha
  rest <- tok word
  pure $ c : rest

-- Parses the datatype of a variable.
--
-- >>> parse typeToken ": int = 3"
-- <Parsed: PInt> <Remaining: "= 3">
typeToken :: Parser Type
typeToken =
  tok (is ':')
    >> (tok (string "int") >> pure PInt)
    ||| (tok (string "string") >> pure PString)

-- Parses the assignment. This is everything including and beyond 
-- the equals sign.
--
-- >>> parse assignmentToken "= 1"
-- <Parsed: Equation (Number 1)> <Remaining: "">
--
-- >>> parse assignmentToken "= 1+2*3"
-- <Parsed: Equation (Plus (Number 1) (Times (Number 2) (Number 3)))> <Remaining: "">
assignmentToken :: Parser Assignment
assignmentToken =
  tok (is '=')
    >> ( --Number <$> (tok integer <* noneof "+-*/") |||
           Equation <$> tok equation
           ||| String <$> tok innerString
       )

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
