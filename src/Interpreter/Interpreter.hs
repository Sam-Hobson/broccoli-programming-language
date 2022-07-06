{-# LANGUAGE FlexibleContexts #-}

module Interpreter where

import Control.Exception (throw)
import Data.Constructors.TH (EqC (eqConstr))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Debug.Trace (trace)
import Exceptions
import InterpreterTypes
import qualified ModuleParser
import qualified ParseTypes as P
import ScopeFuncs
import UsefulFuncs

interpret :: ScopeData -> [P.Statement] -> RetData
interpret sd =
  foldl
    ( \(io, sd', rt) s -> do
        let (io', sd'', rt') = statementIn sd' s
        trace (show (sd', rt) ++ "\n") (mergeIO io io', sd'', fromMaybe rt rt')
        -- (mergeIO io io', sd'', fromMaybe rt rt')
    )
    (pure (), sd, Void)

-- TODO: Incomplete
statementIn :: ScopeData -> P.Statement -> (IO (), ScopeData, Maybe DataType)
statementIn sd (P.FD s) = (pure (), declareFun sd s, Nothing)
statementIn sd (P.FC s) = Nothing <$ evalExpr (P.SymbolCall s) sd
statementIn sd (P.V v) = addLast (declareVar sd v) Nothing
statementIn sd P.Empty = (pure (), sd, Nothing)
statementIn sd (P.Ret r) = Just <$> evalExpr r sd

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

evalFunArgs :: [DataType] -> Namespace -> ScopeData -> [P.Expr] -> (IO (), ScopeData, [DataType])
evalFunArgs dt ns sd e =
  foldl
    ( \(io, sd', types) (expectedType, e') -> do
        let r = evalExpr e' sd'
        if not $ eqConstr expectedType (trd3 r)
          then throw $ MismatchedParameterException $ "Incorrect parameter types in: " ++ show ns ++ " given."
          else (mergeIO io (fst3 r), snd3 r, types ++ [trd3 r])
    )
    (pure (), sd, [])
    (zip dt e)

-- TODO: Incomplete
evalExpr :: P.Expr -> ScopeData -> RetData
evalExpr (P.Symbol s) d = (pure (), d, varLookup s d)
evalExpr (P.Equation e) d = evalEquation e d
evalExpr (P.SymbolCall (n, c)) d = do
  let fdata = trace (show (funLookup n d) ++ "\n") (funLookup n d)
  case fdata of
    FunData ns av rtype code -> do
      let (io, d', argVals) = evalFunArgs (snd <$> av) ns d c
      let d'' = calcFunScope d' fdata
      let d''' = foldl addVar d'' (zip (fst <$> av) argVals)
      let callResults = interpret d''' (content fdata)
      (mergeIO io (fst3 callResults), mergeScopeData (snd3 callResults) d''', trd3 callResults)
    BuiltIn fn input output -> do
      let (io, d', argVals) = evalFunArgs input (traceScope d ++ [fn]) d c
      let (io', d'', retVal) = call fdata argVals d'
      (mergeIO io io', d'', retVal)
evalExpr e d = (pure (), d, evalPrimitiveExpr e)

evalEquation :: P.Equation -> ScopeData -> RetData
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
