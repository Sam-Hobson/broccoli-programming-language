module UsefulFuncs
where

import Data.Map (Map)
import qualified Data.Map as Map

import InterpreterTypes
import Exceptions
import Control.Exception (throw)
import qualified ParseTypes as P

import Debug.Trace
import Data.Type.Equality (inner)

-- Takes a previous IO, and combines it with a new IO. This is used to allow the
-- program to cumulate output.
mergeIO :: IO () -> IO () -> IO ()
mergeIO oldIO newIO = mconcat [oldIO, newIO]

-- This is an empty DefinedData.
emptyData :: DefinedData
emptyData = DefinedData Map.empty Map.empty

fst3 :: (a, b, c) -> a
fst3 (a, _, _) = a

snd3 :: (a, b, c) -> b
snd3 (_, b, _) = b

trd3 :: (a, b, c) -> c
trd3 (_, _, c) = c

addLast :: (a, b) -> c -> (a, b, c)
addLast (a, b) c = (a, b, c)

-- Maps a parse type to an interpreted type.
mapPTypes :: P.Type -> DataType
mapPTypes P.PInt = Int Nothing
mapPTypes P.PString = String Nothing
mapPTypes P.PVoid = Void
