module Unification.Simple where

import Control.Monad.Reader
import Data.Map (Map, (!?))
import Data.Map qualified as Map
import Unification.Expr

type Context = Map Identifier Type

infer :: Expr -> ReaderT Context Maybe Type
infer = \case
  Constant -> pure Const
  Variable identifier ->
    lift =<< asks (!? identifier)
  Let lhs rhs body -> do
    rhsType <- infer rhs
    local (Map.insert lhs rhsType) (infer body)
  Tuple lhs rhs -> do
    lhsType <- infer lhs
    rhsType <- infer rhs
    pure (Pair lhsType rhsType)
  First expr -> do
    (t1, _) <- lift . pairToTuple =<< infer expr
    pure t1
  Second expr -> do
    (_, t2) <- lift . pairToTuple =<< infer expr
    pure t2
