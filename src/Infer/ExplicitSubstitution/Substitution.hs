module Infer.ExplicitSubstitution.Substitution where


import Data.Text (Text)
import qualified Data.Text as T
import Data.Map (Map)
import qualified Data.Map as Map

import Infer.Syntax

newtype Substitution
  = Substitution (Map Text Type)




