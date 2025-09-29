module CPS.ThreeSteps.Step0 where

import Data.Text (Text)

data Exp
  = Trivial Trivial
  | Serious Serious
  deriving (Eq, Ord, Show)

data Serious
  = App Exp Exp
  deriving (Eq, Ord, Show)

data Trivial
  = Var Text
  | Lam Text Exp
  deriving (Eq, Ord, Show)
