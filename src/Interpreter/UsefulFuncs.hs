module UsefulFuncs
where

import InterpreterTypes
import Exceptions
import Control.Exception (throw)

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

traceScope :: ScopeData -> Namespace
traceScope s = scope s : case innerScope s of
                            NoData  -> []
                            _       -> traceScope (innerScope s)

-- Takes 2 ScopeData objects and merges their data.
-- The first ScopeData passed should contain new data, and should
-- be a subset of the scope of the second ScopeData.
mergeScopeData :: ScopeData -> ScopeData -> ScopeData
mergeScopeData a b
    | scope a /= scope b = throw $ ScopeException $ "Function returned with invalid scope: " ++ show (traceScope a)
    | innerScope a /= NoData = a {innerScope = mergeScopeData (innerScope a) (innerScope b)}
    | innerScope a == NoData = a {innerScope = innerScope b}
    | otherwise = throw $ ScopeException $ "Function returned with invalid scope: " ++ show (traceScope a)

