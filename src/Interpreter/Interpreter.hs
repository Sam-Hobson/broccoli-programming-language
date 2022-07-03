{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}

module Interpreter where

-- import qualified Data.HashTable.IO as H

import Control.Exception (throw)
import Data.Data (Data (toConstr), Typeable)
import Data.Map
import Exceptions
import qualified ModuleParser
import qualified ParseTypes as P

data DataType
  = Int Integer
  | String String
  | Void
  deriving (Show, Eq, Typeable, Data)

instance Num DataType where
  Int a + Int b = Int (a + b)
  String a + String b = String (a ++ b)
  _ + _ = throw $ InvalidEquationException "Cannot add datatype."
  Int a - Int b = Int (a - b)
  _ - _ = throw $ InvalidEquationException "Cannot subtract datatype."
  Int a * Int b = Int (a * b)
  _ * _ = throw $ InvalidEquationException "Cannot multiply datatype."
  negate (Int a) = Int (negate a)
  negate _ = throw $ InvalidEquationException "Cannot negate datatype."
  abs (Int a) = Int (abs a)
  abs _ = throw $ InvalidEquationException "Cannot abs datatype."
  signum (Int a) = Int (signum a)
  signum _ = throw $ InvalidEquationException "Cannot signum datatype."
  fromInteger = undefined

type VarMap = [(String, DataType)]

data FunData = FunData {argTypes :: [DataType], retType :: DataType, content :: [P.Statement]}

type FunMap = [(String, FunData)]

data DefinedData = DefinedData {vars :: VarMap, funs :: FunMap}

type Namespace = [String]

data ScopeData = ScopeData {encapsulating :: DefinedData, local :: DefinedData}

type StateData = (Namespace, ScopeData)

global :: StateData
global = (["global"], ScopeData DefinedData DefinedData)

-- TODO: Incomplete
interpret :: StateData -> [P.Statement] -> (IO (), DefinedData, DataType)
interpret = undefined

-- TODO: Incomplete
statementIn :: P.Statement -> IO ()
statementIn (P.FD s) = undefined
statementIn (P.FC s) = undefined
statementIn (P.V (a, b, c)) = undefined
statementIn P.Empty = undefined

lookupLast :: Eq a => a -> [(a, b)] -> Maybe b
lookupLast a l = ll' a l Nothing
  where
    ll' b [] r = r
    ll' b ((c, d) : xs) r
      | b == c = ll' b xs (Just d)
      | otherwise = ll' b xs r

lookupVar :: String -> ScopedData -> DataType
lookupVar s d = case lookupLast s (fst $ snd d) of
  Just dt -> dt
  Nothing -> throw $ UnboundSymbolException $ "Symbol: " ++ s ++ " not bound."

lookupFun :: String -> ScopedData -> FunData
lookupFun s d = case lookupLast s (snd $ snd d) of
  Just dt -> dt
  Nothing -> throw $ UnboundSymbolException $ "Symbol: " ++ s ++ " not bound."

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

sameConstructor :: (Data a1, Data a2) => a1 -> a2 -> Bool
sameConstructor a b = toConstr a == toConstr b

-- TODO: Incomplete
evalExpr :: P.Expr -> ScopedData -> (ScopedData, DataType)
evalExpr P.None d = (d, Void)
evalExpr (P.Constant i) d = (d, Int i)
evalExpr (P.String s) d = (d, String s)
evalExpr (P.Symbol s) d = (d, lookupVar s d)
evalExpr (P.Equation e) d = (d, evalEquation e d)
evalExpr (P.SymbolCall (n, c)) d = do
  let fdata = lookupFun n d
  let argTypes = fst3 fdata
  -- let newData = if length argTypes == length c then
  --       foldr  d (zip argTypes c)
  --       else d
  undefined

-- evalParameter :: (DataType, P.Expr) -> 

evalEquation :: P.Equation -> ScopedData -> DataType
evalEquation (P.Number i) _ = Int i
evalEquation (P.Symbol1 s) d = lookupVar s d
evalEquation (P.Plus e1 e2) d = evalEquation e1 d + evalEquation e2 d
evalEquation (P.Minus e1 e2) d = evalEquation e1 d - evalEquation e2 d
evalEquation (P.Times e1 e2) d = evalEquation e1 d * evalEquation e2 d

-- TODO: Incomplete
compTypeVal :: DataType -> P.Expr -> Bool
compTypeVal (Int _) (P.Constant _) = True
compTypeVal _ _ = False
