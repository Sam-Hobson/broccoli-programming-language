{-# LANGUAGE FlexibleContexts #-}

module Interpreter where

import Control.Exception (throw)
import Control.Applicative ((<|>))
import Data.Map (Map)
import Data.List (isPrefixOf)
import qualified Data.Map as Map
import Data.Constructors.TH ( EqC(eqConstr) )

import InterpreterTypes
import Exceptions
import qualified ModuleParser
import qualified ParseTypes as P

concatDefinedData :: DefinedData -> DefinedData -> DefinedData
concatDefinedData a b = DefinedData (Map.union (vars a) (vars b)) (Map.union (funs a) (funs b))

emptyData :: DefinedData
emptyData = DefinedData Map.empty Map.empty

global :: ScopeData
global = ScopeData "global" emptyData NoData

-- TODO: Incomplete
interpret :: ScopeData -> [P.Statement] -> (IO (), ScopeData, DataType)
interpret = undefined

-- TODO: Incomplete
statementIn :: P.Statement -> IO ()
statementIn (P.FD s) = undefined
statementIn (P.FC s) = undefined
statementIn (P.V (a, b, c)) = undefined
statementIn P.Empty = undefined

scopedLookup :: (DefinedData -> Map String a) -> String -> ScopeData -> a
scopedLookup f s sd = sl sd Nothing
    where
        sl NoData Nothing   = throw $ UnboundSymbolException $ "Symbol: " ++ s ++ " not bound."
        sl NoData (Just a)  = a
        sl sd' a            = sl (innerScope sd') (Map.lookup s (f $ defData sd') <|> a)

varLookup :: String -> ScopeData -> DataType
varLookup = scopedLookup vars

funLookup :: String -> ScopeData -> FunData
funLookup = scopedLookup funs

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
    (zip (argTypes fd) e)

traceScope :: ScopeData -> Namespace
traceScope s = scope s : case innerScope s of
                            NoData  -> []
                            _       -> traceScope (innerScope s)

calcFunScope :: ScopeData -> FunData -> ScopeData
calcFunScope s f = cfs s (funNs f)
    where
        cfs s' (x:y:zs)
            | scope s' /= x = throw $ UnboundSymbolException $ "Function: " ++ show (funNs f) ++ " not found."
            | null zs = s' {innerScope = ScopeData y emptyData NoData}
            | otherwise = s' {innerScope = cfs (innerScope s') (y:zs)}
        cfs _ _ = throw $ ScopeException $ "Function: " ++ show (funNs f) ++ " not in scope."

mergeScopeData :: ScopeData -> ScopeData -> ScopeData
mergeScopeData a b
    | scope a /= scope b = throw $ ScopeException $ "Function returned with invalid scope: " ++ show (traceScope a)
    | innerScope a /= NoData = a {innerScope = mergeScopeData (innerScope a) (innerScope b)}
    | innerScope a == NoData = a {innerScope = innerScope b}
    | otherwise = throw $ ScopeException $ "Function returned with invalid scope: " ++ show (traceScope a)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

-- TODO: Incomplete
evalExpr :: P.Expr -> ScopeData -> (IO (), ScopeData, DataType)
evalExpr P.None d                   = (pure (), d, Void)
evalExpr (P.Number i) d             = (pure (), d, Int i)
evalExpr (P.String s) d             = (pure (), d, String s)
evalExpr (P.Symbol s) d             = (pure (), d, varLookup s d)
evalExpr (P.Equation e) d           = evalEquation e d
evalExpr (P.SymbolCall (n, c)) d    = do
  let fdata = funLookup n d
  let (d', args) = evalFunArgs fdata d c
  let d'' = calcFunScope d' fdata
  let callResults = interpret d'' (content fdata)
  (fst3 callResults, mergeScopeData (snd3 callResults) d'', trd3 callResults)

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
