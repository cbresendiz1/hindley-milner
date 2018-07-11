--{-# OPTIONS_GHC -fwarn-tabs #-}

module Monad (
    StlcError(..),
    EvalM,
    Infer
  ) where

import Control.Monad.Except
import Control.Monad.State

import Syntax

data StlcError = ParserError String
               | OtherError String
               | NotInScope Var
               | FunTypeError Type
               | TypeError Type Type
               | NoRuleApplies
  deriving (Eq)

instance Show StlcError where
    showsPrec _ (ParserError msg)  = showString "Parse Error: " . showString msg
    showsPrec _ (OtherError msg)   = showString "Error: " . showString msg
    showsPrec _ NoRuleApplies      = showString "No rule applies:"
    showsPrec _ (NotInScope v)     = showString "Variable " .
                                     showsPrec 0 v .
                                     showString " is not in scope"
    showsPrec _ (FunTypeError t)   = showString "Expected function type but got type " .
                                     showsPrec 1 t
    showsPrec _ (TypeError t1 t2)  = showString "Expected type " .
                                     showsPrec 1 t2 .
                                     showString " but got type " .
                                     showsPrec 1 t1

--instance Except StlcError where
--    strMsg s = OtherError s
type Infer a = ExceptT StlcError (State [Type]) a 

type Gamma = [(Var,  Type)]

type EvalM a = Either StlcError a
