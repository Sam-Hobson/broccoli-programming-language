module ScopeFuncs
where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception (throw)

import InterpreterTypes
import Exceptions
import UsefulFuncs

addVar :: ScopeData -> (String, DataType) -> ScopeData
addVar sd (k, v) = do
    let dd = defData sd
    sd {defData = dd {vars = Map.insert k v (vars dd)}}

addFun :: ScopeData -> (String, FunData) -> ScopeData
addFun sd (k, v) = do
    let dd = defData sd
    sd {defData = dd {funs = Map.insert k v (funs dd)}}

calcFunScope :: ScopeData -> FunData -> ScopeData
calcFunScope s f = cfs s (funNs f)
  where
    cfs s' (x : y : zs)
      | scope s' /= x = throw $ UnboundSymbolException $ "Function: " ++ show (funNs f) ++ " not found."
      | null zs = s' {innerScope = ScopeData y emptyData NoScope}
      | otherwise = s' {innerScope = cfs (innerScope s') (y : zs)}
    cfs _ _ = throw $ ScopeException $ "Function: " ++ show (funNs f) ++ " not in scope."
