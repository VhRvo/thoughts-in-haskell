module NbE.Closed where

import Data.Map.Strict (Map, insert, (!?))
import Data.Text (Text)
import Prelude hiding (id)

data Type
  = Base Text
  | Func Type Type

data Value
  = VLam Env Text Term

type Env = Map Text Value

data Term
  = Var Text
  | Lam Text Term
  | App Term Term

eval :: Env -> Term -> Maybe Value
eval env = \case
  Var id -> env !? id
  Lam id body -> Just (VLam env id body)
  App fun arg -> do
    fun' <- eval env fun
    arg' <- eval env arg
    apply fun' arg'

apply :: Value -> Value -> Maybe Value
apply fun arg = case fun of
  VLam env id body -> eval (insert id arg env) body
