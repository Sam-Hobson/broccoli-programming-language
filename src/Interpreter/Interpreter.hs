{-# LANGUAGE FlexibleContexts #-}

module Interpreter where

import Data.Maybe (fromMaybe)
import Control.Exception (throw)
import Data.Constructors.TH (EqC (eqConstr))
import Data.Map (Map)
import qualified Data.Map as Map
import Exceptions
import InterpreterTypes
import qualified ModuleParser
import qualified ParseTypes as P
import UsefulFuncs

import Debug.Trace (trace)

interpret :: ScopeData -> [P.Statement] -> (IO (), ScopeData, DataType)
interpret sd = foldl
    (\(io, sd', rt) s -> do
        let (io', sd'', rt') = statementIn sd' s
        trace (show (sd', rt) ++ "\n") (mconcat [io, io'], sd'', fromMaybe rt rt')
    )
    (pure (), sd, Void)

-- TODO: Incomplete
statementIn :: ScopeData -> P.Statement -> (IO (), ScopeData, Maybe DataType)
statementIn sd (P.FD s)     = (pure (), declareFun sd s, Nothing)
statementIn sd (P.FC s)     = Nothing <$ evalExpr (P.SymbolCall s) sd
statementIn sd (P.V v)      = addLast (declareVar sd v) Nothing
statementIn sd P.Empty      = (pure (), sd, Nothing)
statementIn sd (P.Ret r)    = Just <$> evalExpr r sd

addVar :: ScopeData -> (String, DataType) -> ScopeData
addVar sd (k, v) = do
    let dd = defData sd
    sd {defData = dd {vars = Map.insert k v (vars dd)}}

addFun :: ScopeData -> (String, FunData) -> ScopeData
addFun sd (k, v) = do
    let dd = defData sd
    sd {defData = dd {funs = Map.insert k v (funs dd)}}

declareVar :: ScopeData -> P.Var -> (IO (), ScopeData)
declareVar sd (a, b, c) = do
    let (io, sd', dt) = evalExpr c sd
    let sd'' = addVar sd' (a, dt)
    (io, sd'')

declareFun :: ScopeData -> P.Function -> ScopeData
declareFun sd (a, b, c, d) = addFun sd (a, FunData (traceScope sd ++ [a]) (argDefinition <$> b) (mapPTypes c) d)

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

evalFunArgs :: FunData -> ScopeData -> [P.Expr] -> (ScopeData, [(String, DataType)])
evalFunArgs fd sd e =
  foldl
    ( \(sd', types) ((varname, expectedType), e') -> do
        let r = evalExpr e' sd'
        if not $ eqConstr expectedType (trd3 r)
          then throw $ MismatchedParameterException $ "Incorrect parameter types for: " ++ show (funNs fd) ++ " given."
          else (snd3 r, types ++ [(varname, trd3 r)])
    )
    (sd, [])
    (zip (args fd) e)

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
  let d''' = foldl addVar d'' args
  let callResults = interpret d''' (content fdata)
  (fst3 callResults, mergeScopeData (snd3 callResults) d''', trd3 callResults)
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
