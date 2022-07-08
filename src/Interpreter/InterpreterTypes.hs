{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module InterpreterTypes where

import Control.Exception (throw)
import Data.Constructors.TH (deriveEqC)
import Data.Data (Data)
import Data.Map (Map)
import qualified Data.Map as Map
import Exceptions
import qualified ParseTypes as P

data DataType
  = Int (Maybe Integer)
  | String (Maybe String)
  | Void
  deriving
    (Show, Eq, Data)

$(deriveEqC ''DataType)

instance Num DataType where
  Int (Just a) + Int (Just b) = Int (Just $ a + b)
  Int (Just a) + String (Just b) = String (Just $ show a ++ b)
  String (Just a) + Int (Just b) = String (Just $ a ++ show b)
  String (Just a) + String (Just b) = String (Just $ a ++ b)
  _ + _ = throw $ InvalidEquationException "Cannot add datatype."
  Int (Just a) - Int (Just b) = Int (Just $ a - b)
  _ - _ = throw $ InvalidEquationException "Cannot subtract datatype."
  Int (Just a) * Int (Just b) = Int (Just $ a * b)
  _ * _ = throw $ InvalidEquationException "Cannot multiply datatype."
  negate (Int (Just a)) = Int (Just $ negate a)
  negate _ = throw $ InvalidEquationException "Cannot negate datatype."
  abs (Int (Just a)) = Int (Just $ abs a)
  abs _ = throw $ InvalidEquationException "Cannot abs datatype."
  signum (Int (Just a)) = Int (Just $ signum a)
  signum _ = throw $ InvalidEquationException "Cannot signum datatype."
  fromInteger = undefined

type VarMap = Map String DataType

type FunMap = Map String FunData

type Name = String

type Namespace = [Name]

type RetData = (IO (), ScopeData, DataType)

data FunData
  = FunData {funNs :: Namespace, args :: [(String, DataType)], retType :: DataType, content :: [P.Statement]}
  | BuiltIn {fname :: String, itypes :: [DataType], call :: [DataType] -> ScopeData -> RetData}

instance Show FunData where
    show (FunData ns a r c) = "{" ++ show ns ++ ", " ++ show a ++ ", " ++ show r ++ ", " ++ show c ++"}"
    show (BuiltIn n i c) = "{" ++ show n ++ "," ++ show i ++ "}"

instance Eq FunData where
    (FunData a b c d) == (FunData e f g h) = (a == e) && (b == f) && (c == g) && (d == h)
    (BuiltIn a b c) == (BuiltIn d e f) = (a == d) && (b == e)
    _ == _ = False

data DefinedData = DefinedData {vars :: VarMap, funs :: FunMap}
  deriving (Show, Eq)

data ScopeData
  = ScopeData {scope :: Name, defData :: DefinedData, innerScope :: ScopeData}
  | NoScope
  deriving (Show, Eq)
