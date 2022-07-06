module GlobalScope
where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception (throw)

import Exceptions (MismatchedParameterException(MismatchedParameterException))
import InterpreterTypes
import UsefulFuncs
import ScopeFuncs

globalPrint :: FunData
globalPrint = BuiltIn "print" [String Nothing] globalPrintI

globalPrintI :: [DataType] -> ScopeData -> RetData
globalPrintI [String (Just s)] sd = (putStr s, sd, Void)
globalPrintI [Int (Just i)] sd = (putStr $ show i, sd, Void)
globalPrintI s sd
    = throw
        $ MismatchedParameterException
        $ "Incorrect parameter passed to function \'print\'. Expected: String or Int. Got: "
        ++ show s

globalFuns :: FunMap
globalFuns = foldr (uncurry Map.insert) Map.empty
    [("print", globalPrint)]

globalDefData :: DefinedData
globalDefData = DefinedData Map.empty globalFuns

global :: ScopeData
global = ScopeData "global" globalDefData NoScope
