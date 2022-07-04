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
  = Int Integer
  | String String
  | Void
  deriving
    (Show, Eq, Data)
$(deriveEqC ''DataType)

instance Num DataType where
  Int a + Int b = Int (a + b)
  String a + String b = String (a ++ b)
  _ + _ = throw $ InvalidEquationException "Cannot add datatype."
  Int a - Int b = Int (a - b)
  _ - _ = throw $ InvalidEquationException "Cannot subtract datatype."
  Int a * Int b = Int (a * b)
  _ * _ = throw $ InvalidEquationException "Cannot multiply datatype."
  negate (Int a) = Int (negate a)
  negate _ = throw $ InvalidEquationException "Cannot negate datatype."
  abs (Int a) = Int (abs a)
  abs _ = throw $ InvalidEquationException "Cannot abs datatype."
  signum (Int a) = Int (signum a)
  signum _ = throw $ InvalidEquationException "Cannot signum datatype."
  fromInteger = undefined

type VarMap = Map String DataType

type FunMap = Map String FunData

type Name = String

type Namespace = [Name]

data FunData = FunData {funNs :: Namespace, argTypes :: [DataType], retType :: DataType, content :: [P.Statement]}
  deriving (Show, Eq)

data DefinedData = DefinedData {vars :: VarMap, funs :: FunMap}
  deriving (Show, Eq)

data ScopeData
    = ScopeData {scope :: Name, defData :: DefinedData, innerScope :: ScopeData}
    | NoData
  deriving (Show, Eq)
