module GlobalScope where

import Control.Exception (throw)
import Data.Map (Map)
import qualified Data.Map as Map
import Exceptions (MismatchedParameterException (MismatchedParameterException))
import InterpreterTypes
import ScopeFuncs
import UsefulFuncs

globalPrintS :: FunData
globalPrintS = BuiltIn "printS" [String Nothing] globalPrintImplementation

globalPrintI :: FunData
globalPrintI = BuiltIn "printI" [Int Nothing] globalPrintImplementation

globalPrintImplementation :: [DataType] -> ScopeData -> RetData
globalPrintImplementation [String (Just s)] sd = (putStr s, sd, Void)
globalPrintImplementation [Int (Just i)] sd = (putStr $ show i, sd, Void)
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
    [("printS", globalPrintS), ("printI", globalPrintI)]

globalDefData :: DefinedData
globalDefData = DefinedData Map.empty globalFuns

global :: ScopeData
global = ScopeData "global" globalDefData NoScope
