module Interpreter
where

import qualified Data.HashTable.IO as H
import qualified ModuleParser
import ParseTypes (Statement(..))

type Namespace = String

data DataType =
    Int Integer     |
    String String   |
    Void
    deriving (Show)

type HashTable k v = H.BasicHashTable k v

data VarData = VarData {vname :: String, dt :: DataType}
    deriving (Show)

data FunData = FunData {fname :: String, pt :: [DataType], rt :: DataType}
    deriving (Show)

type DefinedData = (IO (HashTable String VarData), IO (HashTable String FunData))

data ScopedData = ScopedData {ns :: Namespace, dd :: DefinedData, sd :: [ScopedData]}


global :: ScopedData
global = ScopedData "global" (H.new, H.new) []

interpret :: [Statement] -> IO ()
interpret = undefined

statementIn :: Statement -> IO ()
statementIn (FD s)  = undefined
statementIn (FC s)  = undefined
statementIn (V s)   = undefined
statementIn Empty   = undefined

