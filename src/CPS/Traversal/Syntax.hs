module CPS.Traversal.Syntax where

import Data.Text (Text)
import qualified Data.Text as T

type Var = Text

data Exp
  = Var Var
  | Lam Var Exp
  | App Exp Exp
