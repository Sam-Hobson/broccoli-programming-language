module GlobalScope where

import Control.Exception (throw)
import Data.Map (Map)
import qualified Data.Map as Map
import Exceptions (MismatchedParameterException (MismatchedParameterException))
import InterpreterTypes
import ScopeFuncs
import UsefulFuncs
import DataTypes

globalPrint :: FunData
globalPrint = BuiltIn "print" [String Nothing] globalPrintImplementation

globalPrintI :: FunData
globalPrintI = BuiltIn "printI" [Int Nothing] globalPrintImplementation

globalPrintB :: FunData
globalPrintB = BuiltIn "printB" [Boolean Nothing] globalPrintImplementation

globalPrintImplementation :: [DataType] -> ScopeData -> RetData
globalPrintImplementation [String (Just s)] sd = (print s, sd, Void)
globalPrintImplementation [Int (Just i)] sd = (print i, sd, Void)
globalPrintImplementation [Boolean (Just b)] sd = (print b, sd, Void)
globalPrintImplementation s sd =
  throw $
    MismatchedParameterException $
      "Incorrect parameter passed to function \'print\'. Expected: String or Int. Got: "
        ++ show s

globalFuns :: FunMap
globalFuns =
  foldr
    (uncurry Map.insert)
    Map.empty
    [ ("print", globalPrint),
      ("printI", globalPrintI),
      ("printB", globalPrintB)
    ]

globalDefData :: DefinedData
globalDefData = DefinedData Map.empty globalFuns

global :: ScopeData
global = ScopeData "global" globalDefData NoScope
