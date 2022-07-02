{-# LANGUAGE InstanceSigs #-}

module Parser where

data ParseError
  = UnexpectedEof
  | ExpectedEof String
  | UnexpectedChar Char
  | UnexpectedString String
  deriving (Eq, Show)

data ParseResult a
  = Error ParseError
  | Result a String
  deriving Eq

newtype Parser a = P { parse :: String -> ParseResult a }

instance Show a => Show (ParseResult a) where
    show (Result a p) = "<Parsed: " ++ show a ++ "> <Remaining: \"" ++ p ++ "\">"
    show (Error UnexpectedEof) = "Unexpected EOF"
    show (Error (ExpectedEof s)) = "Expected EOF: " ++ show s
    show (Error (UnexpectedChar c)) = "Unexpected Char: " ++ show c
    show (Error (UnexpectedString s)) = "Unexpected String: " ++ show s

instance Functor ParseResult where
    fmap f (Result p r) = Result (f p) r
    fmap _ (Error e) = Error e

-- Functor for a Parser
--
-- >>> parse ((+1) <$> (P (\s -> Result 2 s))) "salad"
-- <Parsed: 3> <Remaining: "salad">
instance Functor Parser where
    fmap :: (a -> b) -> Parser a -> Parser b
    fmap f (P a) = P $ (f <$>) <$> a

-- Monad for a parser
instance Monad Parser where
    (>>=) :: Parser a -> (a -> Parser b) -> Parser b
    (>>=) (P p) f = P (
      \i -> case p i of
        Result x rest -> parse (f x) rest
        Error e -> Error e)

-- Applicative for a parser
instance Applicative Parser where
    -- creates a Parser that always succeeds with the given input
    pure :: a -> Parser a
    pure x = P (Result x)

    (<*>) :: Parser (a -> b) -> Parser a -> Parser b
    (<*>) p q = p >>= (\f -> q >>= (pure . f))


-- Some functions

isError :: ParseResult a -> Bool
isError (Error _) = True
isError (Result _ _) = False

getParsed :: ParseResult a -> Either ParseError a
getParsed (Error a) = Left a
getParsed (Result r _) = Right r

getRemainder :: ParseResult a -> Either ParseError String
getRemainder (Error a) = Left a
getRemainder (Result _ r) = Right r

