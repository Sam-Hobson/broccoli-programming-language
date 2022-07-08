module ScopeFuncs
where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception (throw)

import InterpreterTypes
import Exceptions
import UsefulFuncs
import Debug.Trace (trace)

addVar :: ScopeData -> (String, DataType) -> ScopeData
addVar NoScope (k, v) = NoScope
addVar (ScopeData name dd NoScope) (k, v) = ScopeData name (dd {vars = Map.insert k v (vars dd)}) NoScope
addVar sd kv = sd {innerScope = addVar (innerScope sd) kv}

addFun :: ScopeData -> (String, FunData) -> ScopeData
addFun NoScope (k, v) = NoScope
addFun (ScopeData name dd NoScope) (k, v) = ScopeData name (dd {funs = Map.insert k v (funs dd)}) NoScope
addFun sd kv = sd {innerScope = addFun (innerScope sd) kv}

-- Calculates the scope to be passed to a function.
-- This will be the portion of a scope that is defined within the namespace of a function.
calcFunScope :: ScopeData -> FunData -> ScopeData
calcFunScope sd fd = cfs sd (funNs fd)
    where
        cfs sd' ns
            | sd' == NoScope = ScopeData (head ns) emptyData NoScope
            | scope sd' /= head ns = ScopeData (head ns) emptyData NoScope
            | scope sd' == head ns = sd' {innerScope = cfs (innerScope sd') (tail ns)}
            | otherwise = throw $ UnboundSymbolException $ "Function: " ++ show (funNs fd) ++ " not found."
