module ParseTypes
where

data Equation
  = Plus    Equation Equation
  | Minus   Equation Equation
  | Times   Equation Equation
  | Number  Integer
  deriving (Show)

type FunctionData = (String, Expr)

data Expr =
    Constant        Integer         |
    Equation        Equation        |
    String          String          |
    Symbol          String          |
    SymbolCall      FunctionData    |
    None
    deriving (Show)

-- Data types that can be parsed
data Type
  = PInt
  | PString
  deriving (Show)

type Var = (String, Type, Expr)

type Function = (String, [Var], Type, Code)

data Code =
    FD Function     |
    FC FunctionData |
    V Var           |
    Empty
    deriving (Show)

-- data CodeModule
