module Bound.Naive where

import Data.Text (Text)
import Data.List (union, span, (\\))

type Name = Text

data Expr
  = Var Name
  | App Expr Expr
  | Lam Name Expr
  deriving (Eq, Show, Read)

