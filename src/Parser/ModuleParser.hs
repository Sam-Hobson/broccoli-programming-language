module ModuleParser where

import BasicParserFuncs
import ExprLexer (expr, functionCall)
import ParseTypes
import Parser
import SyntaxParserFuncs (idToken, typeToken)
import VarLexer

import Control.Applicative ((<|>))

conditionalIf :: Parser Conditional
conditionalIf = conditionalIf' "if"
  where
    conditionalIf' s =
      do
        tok $ string s
        cond <- surround "(" expr ")"
        content <- surround "{" codeModule "}"
        nextCond <- conditionalIf' "else if" ||| conditionalElse ||| pure (ElseCond [])
        pure $ IfCond cond content nextCond

conditionalElse :: Parser Conditional
conditionalElse = do
    tok $ string "else"
    ElseCond <$> surround "{" codeModule "}"

function :: Parser Function
function = do
  tok $ string "fun"
  name <- idToken
  args <- surround "(" (sepby varDeclaration $ tok $ is ',') ")"
  rtype <- tok (string ":") >> typeToken
  funContent <- surround "{" codeModule "}"
  pure (name, args, rtype, funContent)

ret :: Parser Expr
ret = tok (string "return") >> (expr ||| pure None)

comment :: Parser String
comment = surround "/*" (list $ noneof "*") "*/"

statement :: Parser Statement
statement =
    (comment >> (statement ||| pure Empty))
    ||| (Ret <$> ret)
    ||| (Cond <$> conditionalIf)
    ||| (FD <$> function)
    ||| (FC <$> functionCall)
    ||| (VD <$> varDeclaration)
    ||| (VA <$> varAssignment)

-- Parses code with ';' separating each statement.
--
-- >>> parse codeModule "x: int = 3; y: string = \"salad\"; fun dog(x: int): string {print(hi());};"
-- <Parsed: [V ("x",PInt,Equation (Number 3)),V ("y",PString,String "salad"),FD ("dog",[("x",PInt,None)],PString,[FC ("print",SymbolCall ("hi",None))])]> <Remaining: "">
codeModule :: Parser [Statement]
codeModule = whitespace >> sepby statement (tok $ is ';') <* tok (maybeString ";")
