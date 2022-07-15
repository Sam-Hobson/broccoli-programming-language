{-# LANGUAGE FlexibleContexts #-}

module Interpreter where

import Control.Exception (throw)
import Data.Constructors.TH (EqC (eqConstr))
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import DataTypes
import Debug.Trace (trace)
import Exceptions
import InterpreterTypes
import qualified ModuleParser
import ParseTypes (Expr (Priority))
import qualified ParseTypes as P
import ScopeFuncs
import UsefulFuncs

interpret :: ScopeData -> [P.Statement] -> RetData
interpret sd sa = (a, popFinalScopeData b, c)
  where
    (a, b, c) =
      foldl
        ( \(io, sd', rt) s -> do
            let (io', sd'', rt') = statementIn sd' s
            (mergeIO io io', sd'', fromMaybe rt rt')
        )
        (pure (), sd, Void)
        sa

-- TODO: Incomplete
statementIn :: ScopeData -> P.Statement -> (IO (), ScopeData, Maybe DataType)
statementIn sd (P.FD s) = (pure (), declareFun sd s, Nothing)
statementIn sd (P.FC s) = Nothing <$ evalExpr (P.SymbolCall s) sd
statementIn sd (P.VD (s, t, e)) = addLast (insertVar sd (s, mapPTypes t, e) addVar) Nothing
statementIn sd (P.VA (s, e)) = addLast (insertVar sd (s, varLookup s sd, e) assignVar) Nothing
statementIn sd (P.Ret r) = Just <$> evalExpr r sd
statementIn sd (P.Cond c) = Nothing <$ evalConditional c sd
statementIn sd (P.Loop l) = Nothing <$ evalLoop l sd
statementIn sd P.Empty = (pure (), sd, Nothing)

insertVar :: ScopeData -> (String, DataType, Expr) -> (ScopeData -> (String, DataType) -> ScopeData) -> (IO (), ScopeData)
insertVar sd (s, dt, e) f = do
  let (io, sd', dt') = evalExpr e sd
  if not $ eqConstr dt dt'
    then throw $ NonMatchingTypeException $ "Expected type: " ++ show dt ++ " does not match assigned type: " ++ show dt' ++ "."
    else do
      let sd'' = f sd' (s, dt')
      (io, sd'')

declareFun :: ScopeData -> P.Function -> ScopeData
declareFun sd (a, b, c, d) = addFun sd (a, FunData (traceScope sd ++ [a]) (argDefinition <$> b) (mapPTypes c) d)

argDefinition :: P.Declaration -> (String, DataType)
argDefinition (s, t, e) = (s, f $ evalPrimitiveExpr e)
  where
    f Void = mapPTypes t
    f a = a

evalPrimitiveExpr :: P.Expr -> DataType
evalPrimitiveExpr P.None = Void
evalPrimitiveExpr (P.Number i) = Int $ Just i
evalPrimitiveExpr (P.String s) = String $ Just s
evalPrimitiveExpr (P.Boolean b) = Boolean $ Just b
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

run :: ScopeData -> (ScopeData -> (String, DataType) -> ScopeData) -> [(String, DataType)] -> Name -> [P.Statement] -> RetData
run sd f vars name s = do
  let sd' = calcFunScope sd $ FunData (traceScope sd ++ [name]) [] Void []
  let sd'' = foldl f sd' vars
  let (io, sd''', ret) = interpret sd'' s
  (io, mergeScopeData sd sd''', ret)

runAndMerge :: RetData -> (ScopeData -> (String, DataType) -> ScopeData) -> [(String, DataType)] -> Name -> (DataType -> DataType -> DataType) -> [P.Statement] -> RetData
runAndMerge (io, sd, ret) varf vars name f s = do
  let (io', sd', ret') = run sd varf vars name s
  (mergeIO io io', sd', f ret ret')

-- Evaluates an expression. Expressions can include a call to a function,
-- this will execute the code of a function if it is called.
-- Will merge ScopeData after an expression.
evalExpr :: P.Expr -> ScopeData -> RetData
evalExpr (P.Symbol s) d = (pure (), d, varLookup s d)
evalExpr (P.Equation e) d = evalEquation e d
evalExpr (P.BoolCompOp b) d = evalCompOp b d
evalExpr (P.BoolLogicalOp b) d = evalLogicalOp b d
evalExpr (P.SymbolCall fd) d = evalSymbolCall fd d
evalExpr (Priority e) d = evalExpr e d
evalExpr e d = (pure (), d, evalPrimitiveExpr e)

evalSymbolCall :: P.FunctionData -> ScopeData -> RetData
evalSymbolCall (n, c) d = do
  let (io, d', argVals) = evalFunArgs c d
  let fdata = funLookup n d
  if not $ matchingArgTypes argVals fdata
    then throw $ MismatchedParameterException $ "Incorrect parameter types in: " ++ show (funNs fdata) ++ " given."
    else case fdata of
      FunData ns av rtype code ->
        runAndMerge (io, d', Void) addVar (zip (fst <$> av) argVals) (last $ funNs fdata) const code
      BuiltIn fn input output -> do
        let (io', d'', retVal) = call fdata argVals d'
        (mergeIO io io', d'', retVal)

evalBothSides :: (a -> ScopeData -> RetData) -> ScopeData -> (DataType -> DataType -> DataType) -> a -> a -> RetData
evalBothSides f sd rf a b = do
  let (io, sd', ret) = f a sd
  let (io', sd'', ret') = f b sd'
  (mergeIO io io', sd'', rf ret ret')

-- Evaluates an equation. Operations on DataTypes are
-- handled through the instance of the Num dataclass on
-- DataType.
evalEquation :: P.Equation -> ScopeData -> RetData
evalEquation (P.E e) d = evalExpr e d
evalEquation (P.Plus e1 e2) d = evalBothSides evalEquation d (+) e1 e2
evalEquation (P.Minus e1 e2) d = evalBothSides evalEquation d (-) e1 e2
evalEquation (P.Times e1 e2) d = evalBothSides evalEquation d (*) e1 e2
evalEquation (P.Mod e1 e2) d = evalBothSides evalEquation d mod e1 e2
evalEquation (P.IntDivide e1 e2) d = evalBothSides evalEquation d div e1 e2

putInBool :: (DataType -> DataType -> Bool) -> DataType -> DataType -> DataType
putInBool f a b = Boolean $ Just $ f a b

evalCompOp :: P.BoolCompOp -> ScopeData -> RetData
evalCompOp (P.E1 e) sd = evalExpr e sd
evalCompOp (P.EqOP b1 b2) sd = evalBothSides evalCompOp sd (putInBool (==)) b1 b2
evalCompOp (P.GreaterOP b1 b2) sd = evalBothSides evalCompOp sd (putInBool (>)) b1 b2
evalCompOp (P.GreaterEqOP b1 b2) sd = evalBothSides evalCompOp sd (putInBool (>=)) b1 b2
evalCompOp (P.LessOP b1 b2) sd = evalBothSides evalCompOp sd (putInBool (<)) b1 b2
evalCompOp (P.LessEqOP b1 b2) sd = evalBothSides evalCompOp sd (putInBool (<=)) b1 b2
evalCompOp (P.NotEqOP b1 b2) sd = evalBothSides evalCompOp sd (putInBool (/=)) b1 b2

evalLogicalOp :: P.BoolLogicalOp -> ScopeData -> RetData
evalLogicalOp (P.E2 e) sd = evalExpr e sd
evalLogicalOp (P.AndOP a b) sd = evalBothSides evalLogicalOp sd (putInBool (&&&)) a b
evalLogicalOp (P.OrOP a b) sd = evalBothSides evalLogicalOp sd (putInBool (|||)) a b
evalLogicalOp (P.NotOP a) sd = (Boolean . Just) . (!!!) <$> evalLogicalOp a sd

evalConditional :: P.Conditional -> ScopeData -> RetData
evalConditional (P.IfCond arg code next) sd =
  do
    let (io, sd', ret) = evalExpr arg sd
    case ret of
      Boolean (Just v) ->
        if v
          then runAndMerge (io, sd', ret) addVar [] "if/else if" const code
          else evalConditional next sd'
      v -> throw $ InvalidBooleanException $ "Invalid boolean expression given to if statement. Expressions returned: " ++ show v ++ "."
evalConditional (P.ElseCond code) sd = run sd addVar [] "else" code

evalLoop :: P.Repeating -> ScopeData -> RetData
evalLoop (P.For var cond inc content) sd = undefined
  -- case evalExpr cond sd' of
  --   (io'', sd'', Boolean (Just True)) -> do
  --     let (io''', sd''', _) = run sd'' addVar [] "forLoop" content
  --     let (io'''', sd'''', _) = evalExpr inc sd'''
  --     let (io''''', sd''''', _) = evalLoop (P.For var cond inc content) sd''''
  --     (foldl (flip mergeIO) io' [io'', io''', io'''', io'''''], sd''''', Void)
  --   (io'', sd'', Boolean (Just False)) -> (pure (), sd, Void)
  --   _ -> throw $ InvalidBooleanException "Expected boolean in argument to for loop."
  -- where
  --   (io', sd', _) = statementIn sd (P.VD var)
evalLoop (P.While e content) sd =
  case evalExpr e sd of
    (io, sd', Boolean (Just True)) -> do
      let (io', sd'', _) = run sd' addVar [] "whileLoop" content
      let (io'', sd''', _) = evalLoop (P.While e content) sd''
      (mergeIO (mergeIO io io') io'', sd''', Void)
    (io, sd', Boolean (Just False)) -> (pure (), sd, Void)
    _ -> throw $ InvalidBooleanException "Expected boolean in argument to while loop."
