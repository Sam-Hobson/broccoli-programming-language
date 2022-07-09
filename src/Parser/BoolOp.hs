module BoolOp
where

import ParseTypes
import Parser (Parser, ParseError (UnexpectedString))
import ExprLexer
import BasicParserFuncs
import SyntaxParserFuncs

-- doubleExprOp :: String -> (BoolOp -> BoolOp -> BoolOp) -> Parser BoolOp
-- doubleExprOp s c = do
--     b1 <- tok boolOp
--     tok $ string s
--     b2 <- tok boolOp
--     pure $ c b1 b2

-- eqOP :: Parser BoolOp
-- eqOP = doubleExprOp "==" EqOP

-- greaterOP :: Parser BoolOp
-- greaterOP = doubleExprOp ">" GreaterOP

-- greaterEqOP :: Parser BoolOp
-- greaterEqOP = doubleExprOp ">=" GreaterEqOP

-- lessOP :: Parser BoolOp
-- lessOP = doubleExprOp "<" LessOP

-- lessEqOP :: Parser BoolOp
-- lessEqOP = doubleExprOp "<=" LessEqOP

-- notEqOP :: Parser BoolOp
-- notEqOP = doubleExprOp "!=" NotEqOP

-- andOP :: Parser BoolOp
-- andOP = doubleExprOp "&&" AndOP

-- orOP :: Parser BoolOp
-- orOP = doubleExprOp "||" OrOP

-- xorOP :: Parser BoolOp
-- xorOP = doubleExprOp "^" XorOP

-- notOP :: Parser BoolOp
-- notOP = do
--     tok $ string "not" ||| string "!"
--     NotOP <$> boolOp

-- terminalOP :: Parser BoolOp
-- terminalOP = TerminalOP <$> expr

-- boolOp :: Parser BoolOp
-- boolOp
--     =   eqOP
--     ||| greaterOP
--     ||| greaterEqOP
--     ||| lessOP
--     ||| lessEqOP
--     ||| notEqOP
--     ||| notOP
--     ||| andOP
--     ||| orOP
--     ||| xorOP
--     ||| terminalOP


eqOP :: Parser (BoolOp -> BoolOp -> BoolOp)
eqOP = tok (string "==") >> pure EqOP

greaterOP :: Parser (BoolOp -> BoolOp -> BoolOp)
greaterOP = tok (string ">") >> pure GreaterOP

greaterEqOP :: Parser (BoolOp -> BoolOp -> BoolOp)
greaterEqOP = tok (string ">=") >> pure GreaterEqOP

lessOP :: Parser (BoolOp -> BoolOp -> BoolOp)
lessOP = tok (string "<") >> pure LessOP

lessEqOP :: Parser (BoolOp -> BoolOp -> BoolOp)
lessEqOP = tok (string "<=") >> pure LessEqOP

notEqOP :: Parser (BoolOp -> BoolOp -> BoolOp)
notEqOP = tok (string "!=") >> pure NotEqOP

andOP :: Parser (BoolOp -> BoolOp -> BoolOp)
andOP = tok (string "&&") >> pure AndOP

orOP :: Parser (BoolOp -> BoolOp -> BoolOp)
orOP = tok (string "||") >> pure OrOP

xorOP :: Parser (BoolOp -> BoolOp -> BoolOp)
xorOP = tok (string "^") >> pure XorOP

notOP :: Parser (BoolOp -> BoolOp)
notOP = tok (string "not" ||| string "!") >> pure NotOP

e1 :: Parser BoolOp
e1 = E1 <$> expr


boolOp :: Parser BoolOp
boolOp = do
    let ops = [eqOP, greaterOP, greaterEqOP, lessOP, lessEqOP, notEqOP, andOP, orOP, xorOP]
    r <- foldl chain e1 ops
    case r of
        E1 e -> failed $ UnexpectedString (show e)
        _ -> pure r
