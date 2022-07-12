module SyntaxParserFuncs where

import BasicParserFuncs
import ParseTypes (Type (..))
import Parser

-- Parses the datatype of a variable.
--
-- >>> parse typeToken ": int = 3"
-- <Parsed: PInt> <Remaining: "= 3">
typeToken :: Parser Type
typeToken =
  (tok (string "Int") >> pure PInt)
    ||| (tok (string "String") >> pure PString)
    ||| (tok (string "Bool") >> pure PBool)
    ||| (tok (string "Void") >> pure PVoid)

-- Parses the next word/id. This word can contain
-- numbers but cannot start with one.
--
-- >>> parse idToken "var1: int = 3"
-- <Parsed: "var1"> <Remaining: ": int = 3;">
idToken :: Parser String
idToken = do
  c <- lower
  rest <- tok word
  pure $ c : rest

chain :: Parser a -> Parser (a -> a -> a) -> Parser a
chain p op = p >>= rest
  where
    rest a =
      ( do
          f <- op
          b <- p
          rest (f a b)
      )
        ||| pure a
