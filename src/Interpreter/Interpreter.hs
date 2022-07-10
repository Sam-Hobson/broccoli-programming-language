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
import ParseTypes (Expr(Priority))

interpret :: ScopeData -> [P.Statement] -> RetData
interpret sd sa = (a, popFinalScopeData b, c)
  where
    (a, b, c) =
      foldl
        ( \(io, sd', rt) s -> do
            let (io', sd'', rt') = statementIn sd' s
            -- trace (show (sd', rt) ++ "\n") (mergeIO io io', sd'', fromMaybe rt rt')
            (mergeIO io io', sd'', fromMaybe rt rt')
        )
        (pure (), sd, Void)
        sa

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

evalFunArgs :: [P.Expr] -> ScopeData -> (IO (), ScopeData, [DataType])
evalFunArgs e sd =
  foldl
    ( \(io, sd', types) e' -> do
        let (io', sd'', ret) = evalExpr e' sd'
        (mergeIO io io', sd'', types ++ [ret])
    )
    (pure (), sd, [])
    e

-- Evaluates an expression. Expressions can include a call to a function,
-- this will execute the code of a function if it is called.
-- Will merge ScopeData after an expression.
evalExpr :: P.Expr -> ScopeData -> RetData
evalExpr (P.Symbol s) d = (pure (), d, varLookup s d)
evalExpr (P.Equation e) d = evalEquation e d
evalExpr (P.BoolOp b) d = evalBool b d
evalExpr (P.SymbolCall (n, c)) d = do
  let (io, d', argVals) = evalFunArgs c d
  let fdata = funLookup n d
  if not $ matchingArgTypes argVals fdata
    then throw $ MismatchedParameterException $ "Incorrect parameter types in: " ++ show (funNs fdata) ++ " given."
    else case fdata of
      FunData ns av rtype code -> do
        let d'' = calcFunScope d' fdata
        -- Add all parameter variables to function scope.
        let d''' = foldl addVar d'' (zip (fst <$> av) argVals)
        let callResults = interpret d''' (content fdata) -- Call the function
        (mergeIO io (fst3 callResults), mergeScopeData (snd3 callResults) d', trd3 callResults)
      BuiltIn fn input output -> do
        let (io', d'', retVal) = call fdata argVals d'
        (mergeIO io io', d'', retVal)
evalExpr (Priority e) d = evalExpr e d
evalExpr e d = (pure (), d, evalPrimitiveExpr e)

-- Evaluates an equation. Operations on DataTypes are
-- handled through the instance of the Num dataclass on
-- DataType.
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

comparisonOpMapping :: Ord a => P.BoolOp -> (P.BoolOp, P.BoolOp, a -> a -> Bool)
comparisonOpMapping (P.EqOP a b) = (a, b, (==))
comparisonOpMapping (P.GreaterOP a b) = (a, b, (<))
comparisonOpMapping (P.GreaterEqOP a b) = (a, b, (<=))
comparisonOpMapping (P.LessOP a b) = (a, b, (>))
comparisonOpMapping (P.LessEqOP a b) = (a, b, (>=))
comparisonOpMapping (P.NotEqOP a b) = (a, b, (/=))
comparisonOpMapping a = throw $ InvalidBooleanException $ "Invalid comparison operation: " ++ show a ++ " found."

boolOpMapping :: P.BoolOp -> (P.BoolOp, P.BoolOp, Bool -> Bool -> Bool)
boolOpMapping (P.AndOP a b) = (a, b, (&&))
boolOpMapping (P.OrOP a b) = (a, b, (||))
boolOpMapping a = throw $ InvalidBooleanException $ "Invalid boolean operation: " ++ show a ++ " found."

applyComparisonOp :: Ord a => (a -> a -> Bool) -> Maybe a -> Maybe a -> Bool
applyComparisonOp op b1 b2 = fromMaybe False (op <$> b1 <*> b2)

evalBool :: P.BoolOp -> ScopeData -> RetData
evalBool (P.E1 e) sd = evalExpr e sd
evalBool (P.NotOP b) sd = do
    let (io, sd', Boolean val) = evalBool b sd
    (io, sd', Boolean $ not <$> val)
evalBool b sd = do
    let (b1, b2, op) = comparisonOpMapping b
    let (io, sd', ret) = evalBool b1 sd
    let (io', sd'', ret') = evalBool b2 sd'
    if eqConstr ret ret' then
        (mergeIO io io', sd'', Boolean $ Just $ (applyComparisonOp op) <$> ret <*> ret')
        else
            (mergeIO io io', sd'', Boolean $ Just False)

