module Infer.TypeEnv where

import Data.Coerce
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Infer.Syntax
import Prelude hiding (id)

newtype TypeEnv = TypeEnv (Map Identifier Scheme)

empty :: TypeEnv
empty = TypeEnv Map.empty

lookup :: Identifier -> TypeEnv -> Maybe Scheme
lookup = coerce (Map.lookup @_ @Scheme)

insert :: Identifier -> Scheme -> TypeEnv -> TypeEnv
insert = coerce (Map.insert @_ @Scheme)

keysSet :: TypeEnv -> Set Identifier
keysSet = coerce (Map.keysSet @_ @Scheme)
