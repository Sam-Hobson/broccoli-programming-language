{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module InterpreterTypes
where

import Data.Constructors.TH ( deriveEqC )
import Data.Data (Data)
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Exception (throw)

import qualified ParseTypes as P
import Exceptions

data DataType
  = Int (Maybe Integer)
  | String (Maybe String)
  | Void
  deriving
    (Show, Eq, Data)
$(deriveEqC ''DataType)

instance Num DataType where
  Int (Just a) + Int (Just b) = Int (Just $ a + b)
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

data FunData = FunData {funNs :: Namespace, args :: [(String, DataType)], retType :: DataType, content :: [P.Statement]}
  deriving (Show, Eq)

data DefinedData = DefinedData {vars :: VarMap, funs :: FunMap}
  deriving (Show, Eq)

data ScopeData
    = ScopeData {scope :: Name, defData :: DefinedData, innerScope :: ScopeData}
    | NoData
  deriving (Show, Eq)
