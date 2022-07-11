
module InterpreterTypes where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified ParseTypes as P
import DataTypes

type VarMap = Map String DataType

type FunMap = Map String FunData

type Name = String

type Namespace = [Name]

type RetData = (IO (), ScopeData, DataType)

data FunData
  = FunData {funNs :: Namespace, args :: [(String, DataType)], retType :: DataType, content :: [P.Statement]}
  | BuiltIn {fname :: String, itypes :: [DataType], call :: [DataType] -> ScopeData -> RetData}

instance Show FunData where
  show (FunData ns a r c)   = "{" ++ show ns ++ ", " ++ show a ++ ", " ++ show r ++ ", " ++ show c ++ "}"
  show (BuiltIn n i c)      = "{" ++ show n ++ "," ++ show i ++ "}"

instance Eq FunData where
  (FunData a b c d) == (FunData e f g h)    = (a == e) && (b == f) && (c == g) && (d == h)
  (BuiltIn a b c) == (BuiltIn d e f)        = (a == d) && (b == e)
  _ == _                                    = False

data DefinedData = DefinedData {vars :: VarMap, funs :: FunMap}
  deriving (Show, Eq)

data ScopeData
  = ScopeData {scope :: Name, defData :: DefinedData, innerScope :: ScopeData}
  | NoScope
  deriving (Show, Eq)
