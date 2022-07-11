module ScopeFuncs where

import Control.Applicative ((<|>))
import Control.Exception (throw)
import Data.Constructors.EqC (EqC (eqConstr))
import Data.Map (Map)
import qualified Data.Map as Map
import Debug.Trace (trace)
import Exceptions
import InterpreterTypes
import qualified ParseTypes as P
import UsefulFuncs

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
      | (sd' == NoScope) && null ns = NoScope -- This only applies for surface recursion.
      | sd' == NoScope = ScopeData (head ns) emptyData NoScope
      | scope sd' /= head ns = ScopeData (head ns) emptyData NoScope
      | scope sd' == head ns = sd' {innerScope = cfs (innerScope sd') (tail ns)}
      | otherwise = throw $ UnboundSymbolException $ "Function: " ++ show (funNs fd) ++ " not found."

-- This function will loookup a value within a ScopeData and find the most recent definition
-- of this value.
scopedLookup :: (DefinedData -> Map String a) -> String -> ScopeData -> a
scopedLookup f s sd = sl sd Nothing
  where
    sl NoScope Nothing = throw $ UnboundSymbolException $ "Symbol: " ++ s ++ " not bound."
    sl NoScope (Just a) = a
    sl sd' a = sl (innerScope sd') (Map.lookup s (f $ defData sd') <|> a)

varLookup :: String -> ScopeData -> DataType
varLookup = scopedLookup vars

funLookup :: String -> ScopeData -> FunData
funLookup = scopedLookup funs

-- Returns True if the arguments for the given Fundata accept the types passed
-- in the list of DataTypes.
matchingArgTypes :: [DataType] -> FunData -> Bool
matchingArgTypes d (FunData _ args _ _) = (length d' == length vals) && all (uncurry eqConstr) (zip d' vals)
  where
    vals = snd <$> args
    d' = filter (Void /=) d
matchingArgTypes d (BuiltIn _ args _) = (length d' == length args) && all (uncurry eqConstr) (zip d' args)
  where
    d' = filter (Void /=) d

-- Return the entire namespace of a ScopeData. This is usually use with errors/debugging.
traceScope :: ScopeData -> Namespace
traceScope NoScope = []
traceScope s = scope s : traceScope (innerScope s)

-- Takes 2 ScopeData objects and merges their data.
-- The first ScopeData passed should contain new data, and should
-- be a subset of the scope of the second ScopeData.

mergeScopeData :: ScopeData -> ScopeData -> ScopeData
mergeScopeData newsd oldsd
  | newsd == NoScope = oldsd
  | scope newsd == scope oldsd = newsd {innerScope = mergeScopeData (innerScope newsd) (innerScope oldsd)}
  | otherwise = throw $ ScopeException $ "Function returned with invalid scope: " ++ show (traceScope newsd)

-- Removes the last inner scope data from a scope. This will usually be used when returning
-- from a code segment to remove the segments scope from the ScopeData.
popFinalScopeData :: ScopeData -> ScopeData
popFinalScopeData NoScope = NoScope
popFinalScopeData (ScopeData a b NoScope) = NoScope
popFinalScopeData sd = sd {innerScope = popFinalScopeData (innerScope sd)}

-- Shows a ScopeData in a more readable string. This is used for debugging.
showSD :: ScopeData -> String
showSD sd = show (traceScope sd) ++ showSD' sd 0 ++ "\n\n"
  where
    showSD' (ScopeData a b c) n =
      replicate n '\t' ++ show a ++ "\n\n"
        ++ replicate n '\t'
        ++ show b
        ++ "\n\n"
        ++ showSD' c (n + 1)
    showSD' NoScope n = replicate n '\t' ++ show NoScope ++ "\n"
