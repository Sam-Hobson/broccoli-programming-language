{-# LANGUAGE InstanceSigs #-}

module VarLexer
where

import Parser
import ParserFuncs

-- Data types that can be parsed
data Type =  PInt
            | PString
    deriving Show

newtype Id = Id String
    deriving Show

data Op = TypeID
        | Assignment
    deriving Show

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
    (tok (string "int") >> pure PInt) |||
    (tok (string "string") >> pure PString)

-- Parses an operation token.
--
-- >>> parse opToken ": Int"
-- <Parsed: TypeId> <Remaining: "Int">
--
-- >>> parse opToken "= 3"
-- <Parsed: Assignment> <Remaining: "3">
opToken :: Parser Op
opToken = 
    (tok (is ':') >> pure TypeID) |||
    (tok (is '=') >> pure Assignment)
