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

-- Parses the character at the end of the line.
endLine :: Parser Char
endLine = is ';'

