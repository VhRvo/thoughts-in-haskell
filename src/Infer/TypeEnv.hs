module Infer.TypeEnv where

import Data.Map (Map)
import qualified Data.Map as Map
import Prelude hiding (id)
import Data.Coerce

import Infer.Syntax

newtype TypeEnv = TypeEnv (Map Identifier Type)

lookup :: Identifier -> TypeEnv -> Maybe Type
lookup = coerce (Map.lookup @_ @Type)

insert :: Identifier -> Type -> TypeEnv -> TypeEnv
-- insert id (TypeEnv env) = Map.insert
insert = coerce (Map.insert @_ @Type)
