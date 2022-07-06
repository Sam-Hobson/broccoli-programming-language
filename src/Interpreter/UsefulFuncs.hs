module UsefulFuncs
where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative ((<|>))

import InterpreterTypes
import Exceptions
import Control.Exception (throw)
import qualified ParseTypes as P

import Debug.Trace

mergeIO :: IO () -> IO () -> IO ()
mergeIO oldIO newIO = mconcat [oldIO, newIO]

emptyData :: DefinedData
emptyData = DefinedData Map.empty Map.empty

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

addLast :: (a, b) -> c -> (a, b, c)
addLast (a, b) c = (a, b, c)

traceScope :: ScopeData -> Namespace
traceScope NoScope = []
traceScope s = scope s : traceScope (innerScope s)

-- Takes 2 ScopeData objects and merges their data.
-- The first ScopeData passed should contain new data, and should
-- be a subset of the scope of the second ScopeData.
mergeScopeData :: ScopeData -> ScopeData -> ScopeData
mergeScopeData a b
    | scope a /= scope b = throw $ ScopeException $ "Function returned with invalid scope: " ++ show (traceScope a)
    -- | trace (show b ++ "\n") ((innerScope a) /= NoScope) = a {innerScope = mergeScopeData (innerScope a) (innerScope b)}
    | innerScope a /= NoScope = a {innerScope = mergeScopeData (innerScope a) (innerScope b)}
    -- | trace (show a ++ "\n") (innerScope a == NoScope) = a {innerScope = innerScope b}
    | innerScope a == NoScope = a {innerScope = innerScope b}
    | otherwise = throw $ ScopeException $ "Function returned with invalid scope: " ++ show (traceScope a)

mapPTypes :: P.Type -> DataType
mapPTypes P.PInt = Int Nothing
mapPTypes P.PString = String Nothing
mapPTypes P.PVoid = Void

scopedLookup :: (DefinedData -> Map String a) -> String -> ScopeData -> a
scopedLookup f s sd = sl sd Nothing
    where
        sl NoScope Nothing   = throw $ UnboundSymbolException $ "Symbol: " ++ s ++ " not bound."
        sl NoScope (Just a)  = a
        sl sd' a            = sl (innerScope sd') (Map.lookup s (f $ defData sd') <|> a)

varLookup :: String -> ScopeData -> DataType
varLookup = scopedLookup vars

funLookup :: String -> ScopeData -> FunData
funLookup = scopedLookup funs

