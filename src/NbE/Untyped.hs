module NbE.Untyped where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as Env
import Prelude hiding (id)
import Control.Monad.State

data Type
    = Base Text
    | Func Type Type

data Value
    = VLam Env Text Term
    | VNeutral Neutral

data Neutral
    = NVar Text
    | NApp Neutral Value

type Env = Map Text Value

data Term
    = Var Text
    | Lam Text Term
    | App Term Term
  deriving (Show)

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
  VLam env id body -> eval (Env.insert id arg env) body
  VNeutral neutral -> pure (VNeutral (NApp neutral arg))

reifyNeutral :: Neutral -> StateT Int Maybe Term
reifyNeutral = \case
  NVar id  -> pure (Var id)
  NApp neutral arg ->
    App <$> reifyNeutral neutral <*> reify arg

reify :: Value -> StateT Int Maybe Term
reify = \case
--   fun@(VLam env id body) -> do
  fun@(VLam {}) -> do
    index <- get
    modify' (+ 1)
    let var = "$" <> T.pack (show index)
    -- let env' = Env.insert id (VNeutral (NVar var)) env
    -- body' <- lift $ eval env' body
    body' <- lift $ apply fun (VNeutral (NVar var))
    Lam var <$> reify body'
  VNeutral neutral -> reifyNeutral neutral

normalize :: [Text] -> Term -> Maybe Term
normalize frees term = (`evalStateT` 0) $ do
    value <- lift $ eval (Env.fromList ((\var -> (var, VNeutral (NVar var))) <$> frees)) term
    reify value

demo :: Maybe Term
demo = normalize [] (Lam "x" (App (Lam "y" (Lam "x" (Var "y"))) (Var "x")))

