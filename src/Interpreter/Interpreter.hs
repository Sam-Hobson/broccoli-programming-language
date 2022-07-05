{-# LANGUAGE FlexibleContexts #-}

module Interpreter where

import Control.Exception (throw)
import Data.Constructors.TH (EqC (eqConstr))
import Data.Map (Map)
import qualified Data.Map as Map
import Exceptions
import InterpreterTypes
import qualified ModuleParser
import qualified ParseTypes as P
import UsefulFuncs

-- TODO: Incomplete
interpret :: ScopeData -> [P.Statement] -> (IO (), ScopeData, DataType)
interpret = undefined

-- TODO: Incomplete
statementIn :: ScopeData -> P.Statement -> (IO (), ScopeData)
statementIn sd (P.FD s) = (pure (), declareFun sd s)
statementIn sd (P.FC s) = undefined
statementIn sd (P.V v) = declareVar sd v
statementIn sd P.Empty = undefined

declareVar :: ScopeData -> P.Var -> (IO (), ScopeData)
declareVar sd (a, b, c) = do
    let (io, sd', dt) = evalExpr c sd
    let dd = defData sd'
    let sd'' = sd' {defData = dd {vars = Map.insert a dt (vars dd)}}
    (io, sd'')

declareFun :: ScopeData -> P.Function -> ScopeData
declareFun sd (a, b, c, d) = sd {defData = dd {funs = Map.insert k v (funs dd)}}
  where
    dd = defData sd
    k = a
    v = FunData (traceScope sd ++ [a]) (argDefinition <$> b) (mapPTypes c) d

argDefinition :: P.Var -> (String, DataType)
argDefinition (s, t, e) = (s, f $ evalPrimitiveExpr e)
  where
    f Void = mapPTypes t
    f a = a

evalPrimitiveExpr :: P.Expr -> DataType
evalPrimitiveExpr P.None = Void
evalPrimitiveExpr (P.Number i) = Int $ Just i
evalPrimitiveExpr (P.String s) = String $ Just s
evalPrimitiveExpr a = throw $ ExpectedPrimitiveTypeException $ "Expected primitive type. " ++ show a ++ " provided instead."

evalFunArgs :: FunData -> ScopeData -> [P.Expr] -> (ScopeData, [DataType])
evalFunArgs fd sd e =
  foldl
    ( \(sd', types) (expectedType, e') -> do
        let r = evalExpr e' sd'
        if not $ eqConstr expectedType (trd3 r)
          then throw $ MismatchedParameterException $ "Incorrect parameter types for: " ++ show (funNs fd) ++ " given."
          else (snd3 r, types ++ [trd3 r])
    )
    (sd, [])
    (zip (snd <$> args fd) e)

calcFunScope :: ScopeData -> FunData -> ScopeData
calcFunScope s f = cfs s (funNs f)
  where
    cfs s' (x : y : zs)
      | scope s' /= x = throw $ UnboundSymbolException $ "Function: " ++ show (funNs f) ++ " not found."
      | null zs = s' {innerScope = ScopeData y emptyData NoData}
      | otherwise = s' {innerScope = cfs (innerScope s') (y : zs)}
    cfs _ _ = throw $ ScopeException $ "Function: " ++ show (funNs f) ++ " not in scope."

-- TODO: Incomplete
evalExpr :: P.Expr -> ScopeData -> (IO (), ScopeData, DataType)
evalExpr (P.Symbol s) d = (pure (), d, varLookup s d)
evalExpr (P.Equation e) d = evalEquation e d
evalExpr (P.SymbolCall (n, c)) d = do
  let fdata = funLookup n d
  let (d', args) = evalFunArgs fdata d c
  let d'' = calcFunScope d' fdata
  let callResults = interpret d'' (content fdata)
  (fst3 callResults, mergeScopeData (snd3 callResults) d'', trd3 callResults)
evalExpr e d = (pure (), d, evalPrimitiveExpr e)

evalEquation :: P.Equation -> ScopeData -> (IO (), ScopeData, DataType)
evalEquation (P.E e) d = evalExpr e d
evalEquation (P.Plus e1 e2) d = do
  let r1 = evalEquation e1 d
  let r2 = evalEquation e2 (snd3 r1)
  (pure (), snd3 r2, trd3 r1 + trd3 r2)
evalEquation (P.Minus e1 e2) d = do
  let r1 = evalEquation e1 d
  let r2 = evalEquation e2 (snd3 r1)
  (pure (), snd3 r2, trd3 r1 - trd3 r2)
evalEquation (P.Times e1 e2) d = do
  let r1 = evalEquation e1 d
  let r2 = evalEquation e2 (snd3 r1)
  (pure (), snd3 r2, trd3 r1 * trd3 r2)
