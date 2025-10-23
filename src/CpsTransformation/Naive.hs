module CpsTransformation.Naive where

import Data.Text (Text)
import qualified Data.Text as T
import Control.Monad.State.Strict
import Data.Bifunctor

type Var = Text

data Exp
  = Var Var
  | Lam Var Exp
  | App Exp Exp
  deriving (Eq, Ord, Show)

newK :: State (Int, Int) Var
newK = do
  kIndex <- snd <$> get
  modify' (second succ)
  pure $ "k'" <> T.show kIndex

newFunArgK :: State (Int, Int) (Var, Var, Var)
newFunArgK = do
  (aIndex, kIndex) <- get
  modify' (bimap succ succ)
  pure ("fun'" <> T.show aIndex, "arg'" <> T.show aIndex, "k'" <> T.show kIndex)

naive :: Exp -> State (Int, Int) Exp
naive =
  \case
    Var var -> do
      k <- newK
      pure $ Lam k (App (Var k) (Var var))
    Lam var body -> do
      k <- newK
      body' <- naive body
      pure $ App (Var k) (Lam var body')
    App fun arg -> do
      (m, n, k) <- newFunArgK
      fun' <- naive fun
      arg' <- naive arg
      pure $ Lam k $ App fun' (Lam m (App arg' (Lam n (App (App (Var m) (Var n)) (Var k)))))





