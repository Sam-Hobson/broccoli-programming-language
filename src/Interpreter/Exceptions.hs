module Exceptions where

import Control.Exception (Exception)
import Data.Data (Typeable)

newtype UnboundSymbolException
  = UnboundSymbolException String
  deriving (Show, Typeable)

instance Exception UnboundSymbolException

newtype ScopeException
  = ScopeException String
  deriving (Show, Typeable)

instance Exception ScopeException

newtype InvalidEquationException
  = InvalidEquationException String
  deriving (Show, Typeable)

instance Exception InvalidEquationException

newtype MismatchedParameterException
  = MismatchedParameterException String
  deriving (Show, Typeable)

instance Exception MismatchedParameterException

