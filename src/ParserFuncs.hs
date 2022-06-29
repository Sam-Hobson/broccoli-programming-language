module ParserFuncs where

import Data.Char
import Parser

-- Return a parser that always fails with the given error.
failed :: ParseError -> Parser a
failed e = P (\x -> Error e)

-- Produces a parser that always fails with 'UnexpectedChar' using the given
-- character.
unexpectedCharParser :: Char -> Parser a
unexpectedCharParser c = P $ const (Error (UnexpectedChar c))

-- Return a parser that succeeds with a character off the input or fails with
-- an error if the input is empty.
character :: Parser Char
character = P f
  where
    f "" = Error UnexpectedEof
    f (x : xs) = Result x xs

-- Check if there is any input left to parse.
eof :: Parser ()
eof = P f
  where
    f "" = Result () ""
    f (x : xs) = Error (UnexpectedChar x)

-- Return a parser that tries the first parser for a successful value, then:
(|||) :: Parser a -> Parser a -> Parser a
p1 ||| p2 =
  P $ \s ->
    case parse p1 s of
      Error _ -> parse p2 s
      a -> a

infixl 3 |||

-- Return a parser that continues producing a list of values from the given
-- parser.
list :: Parser a -> Parser [a]
list p = list1 p ||| pure []

-- Return a parser that produces at least one value from the given parser
-- then continues producing a list of values from the given parser (to
-- ultimately produce a non-empty list).
list1 :: Parser a -> Parser [a]
list1 p = p >>= (\p' -> list p >>= (\p'' -> pure (p' : p'')))

-- Return a parser that produces a character but fails if the given function is not satisfied.
satisfy :: (Char -> Bool) -> Parser Char
satisfy f = do
  v <- character
  let next =
        if f v
          then pure
          else const $ unexpectedCharParser v
  next v

-- Return a parser that produces the given character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not equal to the given character.
is :: Char -> Parser Char
is c = satisfy (== c)

-- Return a parser that produces any character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is equal to the given character.
isNot :: Char -> Parser Char
isNot c = satisfy (/= c)

-- | Return a parser that produces a character between '0' and '9' but fails if
--
--   * the input is empty; or
--
--   * the produced character is not a digit.
digit :: Parser Char
digit = satisfy isDigit

-- | Return a parser that produces a space character but fails if
--
--   * the input is empty; or
--
--   * the produced character is not a space.
space :: Parser Char
space = satisfy isSpace

-- Same as space, but for multiple spaces.
spaces :: Parser String
spaces = list (is ' ')

-- Return a parser that produces one or more space characters (consuming
-- until the first non-space) but fails if:
--
--   * the input is empty; or
--
--   * the first produced character is not a space.
spaces1 :: Parser String
spaces1 = list1 (is ' ')

-- Return a parser that produces a lower-case character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not lower-case.
lower :: Parser Char
lower = satisfy isLower

-- Return a parser that produces an upper-case character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not upper-case.
upper :: Parser Char
upper = satisfy isUpper

-- Return a parser that produces an alpha character but fails if:
--
--   * the input is empty; or
--
--   * the produced character is not alpha.
alpha :: Parser Char
alpha = satisfy isAlpha

-- Return a parser that sequences the given list of parsers by producing all
-- their results but fails on the first failing parser of the list.
--
-- We want any character, followed by lower case @x@, then any upper case
-- letter.
--
-- >>> seq = [character, is 'x', upper]
-- >>> parse (sequenceParser seq) "axCdef"
-- <Parsed: "axC"> <Remaining: "def">
--
-- >>> isError (parse (sequenceParser seq) "abCdef")
-- True
sequenceParser :: [Parser a] -> Parser [a]
sequenceParser = sequence

-- Return a parser that produces the given number of values off the given
-- parser. This parser fails if the given parser fails in the attempt to
-- produce the given number of values.
--
-- >>> parse (thisMany 4 upper) "ABCDef"
-- <Parsed: "ABCD"> <Remaining: "ef">
--
-- >>> isError (parse (thisMany 4 upper) "ABcDef")
-- True
thisMany :: Int -> Parser a -> Parser [a]
thisMany n l = sequenceParser (replicate n l)

-- A function that parses the given string (fails otherwise).
--
-- >>> parse (string "abc") "abcdef"
-- <Parsed: "abc"> <Remaining: "def">
--
-- >>> isError (parse (string "abc") "bcdef")
-- True
string :: String -> Parser String
string = traverse is

-- A function that applies the given parser, then parses 0 or more
-- spaces, then produces the result of the original parser.
--
-- >>> parse (tok (is 'a')) "a bc"
-- <Parsed: 'a'> <Remaining: "bc">
--
-- >>> parse (tok (is 'a')) "abc"
-- <Parsed: 'a'> <Remaining: "bc">
tok :: Parser a -> Parser a
tok p = do
  a <- p
  spaces
  pure a

-- A function that parses the given char followed by 0 or more spaces.
--
-- >>> parse (charTok 'a') "abc"
-- <Parsed: 'a'> <Remaining: "bc">
--
-- >>> isError (parse (charTok 'a') "dabc")
-- True
charTok :: Char -> Parser Char
charTok = tok . is

-- >>> parse commaTok ",123"
-- <Parsed: ','> <Remaining: "123">
--
-- >>> isError ( parse commaTok "1,23")
-- True
commaTok :: Parser Char
commaTok = charTok ','

-- >>> parse (stringTok "abc") "abc  "
-- <Parsed: "abc"> <Remaining: "">
--
-- >>> isError (parse (stringTok "abc") "bc  ")
-- True
stringTok :: String -> Parser String
stringTok = tok . string

-- >>> parse (sepby1 character (is ',')) "a"
-- <Parsed: "a"> <Remaining: "">
--
-- >>> parse (sepby1 character (is ',')) "a,b,c"
-- <Parsed: "abc"> <Remaining: "">
--
-- >>> parse (sepby1 character (is ',')) "a,b,c,,def"
-- <Parsed: "abc,"> <Remaining: "def">
--
-- >>> isError (parse (sepby1 character (is ',')) "")
-- True
sepby1 :: Parser a -> Parser s -> Parser [a]
sepby1 pa ps = (:) <$> pa <*> list (ps >> pa)

-- >>> parse (sepby character (is ',')) ""
-- <Parsed: ""> <Remaining: "">
--
-- >>> parse (sepby character (is ',')) "a"
-- <Parsed: "a"> <Remaining: "">
--
-- >>> parse (sepby character (is ',')) "a,b,c"
-- <Parsed: "abc"> <Remaining: "">
--
-- >>> parse (sepby character (is ',')) "a,b,c,,def"
-- <Parsed: "abc,"> <Remaining: "def">
sepby :: Parser a -> Parser s -> Parser [a]
sepby pa ps = sepby1 pa ps ||| pure []

-- >>> parse (oneof "abc") "bcdef"
-- <Parsed: 'b'> <Remaining: "cdef">
--
-- >>> isError (parse (oneof "abc") "def")
-- True
oneof :: String -> Parser Char
oneof s = satisfy (`elem` s)

-- >>> parse (noneof "bcd") "abc"
-- <Parsed: 'a'> <Remaining: "bc">
--
-- >>> isError (parse (noneof "abcd") "abc")
-- True
noneof :: String -> Parser Char
noneof s = satisfy (`notElem` s)

spaces2 :: Parser ()
spaces2 = (is ' ' >> spaces2) ||| pure ()

word :: Parser String
word = list (alpha ||| digit)

-- Parse a string if it exists, otherwise, parse nothing. This function
-- wont produce any parseResult errors.
maybeString :: String -> Parser String
maybeString s = string s ||| string ""

-- Reads an integer from input and converts the integer part to a number.
readInt :: String -> Maybe (Integer, String)
readInt s = case reads s of
  [(x, rest)] -> Just (x, rest)
  _ -> Nothing

-- Parse an integer (including the -).
integer :: Parser Integer
integer = do
  m <- maybeString "-"
  maybeString "+"
  num <- list digit
  p $ readInt (m ++ num)
  where
    p (Just (x, _)) = pure x
    p Nothing = P $ const $ Error UnexpectedEof
