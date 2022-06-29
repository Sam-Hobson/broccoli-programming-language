{-# LANGUAGE InstanceSigs #-}

module VarLexer where

import ExprLexer
import Parser
import ParserFuncs

-- Data types that can be parsed
data Type
  = PInt
  | PString
  deriving (Show)

newtype Id = Id String
  deriving (Show)

newtype Assignment = Assignment Expr
  deriving (Show)

newtype TypeID = TypeID Type
  deriving (Show)

data Var = Var Id TypeID Assignment
  deriving (Show)

-- Parses the next word/id. This word can contain
-- numbers but cannot start with one.
--
-- >>> parse idToken "var1: int = 3"
-- <Parsed: Id "var1"> <Remaining: ": int = 3">
idToken :: Parser Id
idToken = do
  c <- alpha
  rest <- tok word
  pure $ Id $ c : rest

-- Parses the datatype of a variable.
--
-- >>> parse typeToken "int = 3"
-- <Parsed: PInt> <Remaining: "= 3">
typeToken :: Parser Type
typeToken =
  (tok (string "int") >> pure PInt)
    ||| (tok (string "string") >> pure PString)

-- Parses an operation token.
--
-- >>> parse opToken ": Int"
-- <Parsed: TypeId> <Remaining: "Int">
--
-- >>> parse opToken "= 3"
-- <Parsed: Assignment> <Remaining: "3">
typeIDToken :: Parser TypeID
typeIDToken = tok (is ':') >> TypeID <$> typeToken

assignmentToken :: Parser Assignment
assignmentToken = tok (is '=') >> Assignment <$> expr

-- Parses a full variable.
--
-- >>> parse varParser "num1: int = 2"
-- <Parsed: Var (Id "num1") (TypeID PInt) (Assignment (Number 2))> <Remaining: "">
--
-- >>> isError (parse varParser "num1: int = hi")
-- True
varParser :: Parser Var
varParser = do
    spaces
    a <- idToken
    b <- typeIDToken
    Var a b <$> assignmentToken
