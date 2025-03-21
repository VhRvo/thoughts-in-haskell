module NbE.Reconstructing.Choice12 where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Control.Monad.Reader as R
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

data Type
  = TBase Text
  | TArrow Type Type

data Expr
  = EBase
  | EVar Text
  | ELam Text Expr
  | EApp Expr Expr

data Value
  = VBase
  | VLam (Value -> Interpreter Value)
  | VNeutral Neutral

data Neutral
  = NVar Text
  | NApp Neutral Normal

data Normal
  = NBase
  | NLam Text Normal
  | NNeutral Neutral

type Env = Map Text Value

newtype Interpreter a
  = Interpreter {runInterpreter :: ReaderT Env (StateT Int (Either Text)) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadState Int,
      MonadError Text
    )

newVar :: Interpreter Text
newVar = do
  index <- get
  modify' (+1)
  pure . T.pack $ "x" <> show index

eval :: Expr -> Interpreter Value
eval = \case
  EBase ->
    pure VBase
  EVar var -> do
    env <- ask
    liftEither (maybe (Left "variable not found") Right . Map.lookup var $ env)
  ELam var body -> do
    env <- ask
    pure $ VLam (\arg -> local (const (Map.insert var arg env)) (eval body))
  EApp e1 e2 -> do
    vf <- eval e1
    va <- eval e2
    doApp vf va

doApp :: Value -> Value -> Interpreter Value
doApp v1 v2 = case v1 of
  VBase ->
    throwError "base in application"
  VLam f ->
    f v2
  VNeutral neutral -> do
    na <- reifyValue v2
    pure (VNeutral (NApp neutral na))

reifyValue :: Value -> Interpreter Normal
reifyValue = \case
  VBase ->
    pure NBase
  VLam f -> do
    var <- newVar
    body <- f (VNeutral (NVar var))
    NLam var <$> reifyValue body
  VNeutral neutral ->
    reifyNeutral neutral

reifyNeutral :: Neutral -> Interpreter Normal
reifyNeutral = pure . NNeutral




