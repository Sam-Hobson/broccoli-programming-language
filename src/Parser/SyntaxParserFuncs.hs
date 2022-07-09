module SyntaxParserFuncs 
where

import Parser
import BasicParserFuncs
import ParseTypes ( Type(..) )

-- Parses the datatype of a variable.
--
-- >>> parse typeToken ": int = 3"
-- <Parsed: PInt> <Remaining: "= 3">
typeToken :: Parser Type
typeToken =
  tok (is ':')
    >> (tok (string "int") >> pure PInt)
    ||| (tok (string "string") >> pure PString)
    ||| (tok (string "void") >> pure PVoid)

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
