{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell #-}

module DataTypes
where

import Data.Constructors.TH (deriveEqC)
import Data.Data (Data)
import Exceptions
import Control.Exception (throw)
import Data.Maybe (fromJust)
import Data.Tuple (swap)

data DataType
  = Int (Maybe Integer)
  | String (Maybe String)
  | Boolean (Maybe Bool)
  | Void
  deriving
    (Show, Eq, Data)

$(deriveEqC ''DataType)

class LogicalBool a where
    (&&&)   :: a -> a -> Bool
    (|||)   :: a -> a -> Bool
    (!!!)   :: a -> Bool

instance LogicalBool DataType where
    Boolean (Just a) &&& Boolean (Just b) = a && b
    a &&& b = throw $ InvalidBooleanException $ "Invalid comparison attempted (&&&) between: " ++ show a ++ " and " ++ show b ++ "."
    Boolean (Just a) ||| Boolean (Just b) = a || b
    a ||| b = throw $ InvalidBooleanException $ "Invalid comparison attempted (|||) between: " ++ show a ++ " and " ++ show b ++ "."
    (!!!) (Boolean (Just a)) = not a
    (!!!) a = throw $ InvalidBooleanException $ "Invalid comparison attempted (!!!): " ++ show a ++ "."

instance Real DataType where
    toRational (Int (Just a)) = toRational a
    toRational a = throw $ InvalidEquationException $ "Invalid math operator used on data: " ++ show a ++ "."

instance Enum DataType where
    fromEnum = fromJust . flip lookup table
    toEnum = fromJust . flip lookup (map swap table)
table :: [(DataType, Int)]
table = [(Int Nothing, 0), (String Nothing, 1), (Boolean Nothing, 2), (Void, 3)]

instance Integral DataType where
    quotRem (Int (Just a)) (Int (Just b)) = (Int (Just (a `div` b)), Int (Just (a `mod` b)))
    quotRem a b = throw $ InvalidEquationException $ "Invalid math operator used between: " ++ show a ++ ", " ++ show b ++ "."
    toInteger (Int (Just a)) = toInteger a
    toInteger a = throw $ InvalidEquationException $ "Invalid math operator used on data: " ++ show a ++ "."

instance Ord DataType where
    Int (Just a) <= Int (Just b)            = a <= b
    String (Just a) <= String (Just b)      = a <= b
    Boolean (Just a) <= Boolean (Just b)    = a <= b
    a <= b = throw $ InvalidBooleanException $ "Invalid comparison attempted between: " ++ show a ++ " and " ++ show b ++ "."

instance Num DataType where
  Int (Just a) + Int (Just b)       = Int (Just $ a + b)
  Int (Just a) + String (Just b)    = String (Just $ show a ++ b)
  String (Just a) + Int (Just b)    = String (Just $ a ++ show b)
  String (Just a) + String (Just b) = String (Just $ a ++ b)
  _ + _ = throw $ InvalidEquationException "Cannot add datatype."
  Int (Just a) - Int (Just b)       = Int (Just $ a - b)
  _ - _ = throw $ InvalidEquationException "Cannot subtract datatype."
  Int (Just a) * Int (Just b)       = Int (Just $ a * b)
  _ * _ = throw $ InvalidEquationException "Cannot multiply datatype."
  negate (Int (Just a))             = Int (Just $ negate a)
  negate _ = throw $ InvalidEquationException "Cannot negate datatype."
  abs (Int (Just a))                = Int (Just $ abs a)
  abs _ = throw $ InvalidEquationException "Cannot abs datatype."
  signum (Int (Just a))             = Int (Just $ signum a)
  signum _ = throw $ InvalidEquationException "Cannot signum datatype."
  fromInteger                       = undefined
