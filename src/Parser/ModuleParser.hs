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


while :: Parser Repeating
while = do
    tok $ string "while"
    r <- tok $ surround "(" expr ")"
    c <- tok $ surround "{" codeModule "}"
    pure $ While r c

for :: Parser Repeating
for = do
    tok $ string "for"
    (dec, cond, inc) <- tok $ surround "(" p ")"
    c <- tok $ surround "{" codeModule "}"
    pure $ For dec cond inc c
    where
        p = do
            a <- tok varDeclaration
            tok $ string ","
            b <- tok expr
            tok $ string ","
            c <- tok expr
            pure (a, b, c)

statement :: Parser Statement
statement =
    (comment >> (statement ||| pure Empty))
    ||| (Ret <$> ret)
    ||| (Loop <$> while)
    ||| (Loop <$> for)
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
