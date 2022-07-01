module ModuleParser
where

import Parser
import BasicParserFuncs
import SyntaxParserFuncs (idToken, typeToken)
import ParseTypes (Code(..), Function)
import ExprLexer (functionCall)
import VarLexer (varDeclaration, varInitialisation, varParser)

function :: Parser Function
function = do
    tok $ string "fun"
    name <- idToken
    args <- surround "(" (sepby varParser (tok (is ','))) ")"
    rtype <- typeToken
    funContent <- surround "{" code "}"
    pure (name, args, rtype, funContent)

code :: Parser Code
code =
    (FD <$> function)       |||
    (FC <$> functionCall)   |||
    (V <$> varParser)       |||
    pure Empty

