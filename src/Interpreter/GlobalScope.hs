module GlobalScope
where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception (throw)

import Exceptions (MismatchedParameterException(MismatchedParameterException))
import InterpreterTypes
import UsefulFuncs
import ScopeFuncs

globalPrintS :: FunData
globalPrintS = BuiltIn "print" [String Nothing] globalPrintImplementation
globalPrintI :: FunData
globalPrintI = BuiltIn "print" [Int Nothing] globalPrintImplementation

globalPrintImplementation :: [DataType] -> ScopeData -> RetData
globalPrintImplementation [String (Just s)] sd = (putStr s, sd, Void)
globalPrintImplementation [Int (Just i)] sd = (putStr $ show i, sd, Void)
globalPrintImplementation s sd
    = throw
        $ MismatchedParameterException
        $ "Incorrect parameter passed to function \'print\'. Expected: String or Int. Got: "
        ++ show s

globalFuns :: FunMap
globalFuns = foldr (uncurry Map.insert) Map.empty
    [("print", globalPrintS), ("print", globalPrintI)]

globalDefData :: DefinedData
globalDefData = DefinedData Map.empty globalFuns

global :: ScopeData
global = ScopeData "global" globalDefData NoScope
