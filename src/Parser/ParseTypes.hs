module ParseTypes where

data Equation
  = Plus Equation Equation
  | Minus Equation Equation
  | Times Equation Equation
  | E Expr
  deriving (Show, Eq)

type FunctionData = (String, [Expr])

data BoolOp
    = EqOP        BoolOp BoolOp
    | GreaterOP   BoolOp BoolOp
    | GreaterEqOP BoolOp BoolOp
    | LessOP      BoolOp BoolOp
    | LessEqOP    BoolOp BoolOp
    | NotEqOP     BoolOp BoolOp
    | AndOP       BoolOp BoolOp
    | OrOP        BoolOp BoolOp
    | XorOP       BoolOp BoolOp
    | NotOP       BoolOp
    | E1          Expr
    deriving (Show, Eq)

data Expr
  = Number Integer
  | Equation Equation
  | BoolOp BoolOp
  | String String
  | Symbol String
  | SymbolCall FunctionData
  | None
  deriving (Show, Eq)

-- Data types that can be parsed
data Type
  = PInt
  | PString
  | PVoid
  deriving (Show, Eq)

type Var = (String, Type, Expr)

type Function = (String, [Var], Type, [Statement])

data Statement
  = FD Function
  | FC FunctionData
  | V Var
  | Ret Expr
  | Empty
  deriving (Show, Eq)
