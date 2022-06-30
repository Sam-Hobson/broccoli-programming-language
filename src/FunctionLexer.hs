module FunctionLexer
where

import Parser
import VarLexer
import SyntaxParserFuncs

data Function = Function {
        name        :: String,
        params      :: [Var],
        returnType  :: Type
        -- content
    }
    deriving (Show)
