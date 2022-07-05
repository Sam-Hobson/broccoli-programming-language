module GlobalScope
where

import InterpreterTypes
import UsefulFuncs

global :: ScopeData
global = ScopeData "global" emptyData NoScope
