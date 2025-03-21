module NbE.Reconstructing.Choice11 where

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
  | NApp Neutral Value

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
  VNeutral neutral ->
    pure (VNeutral (NApp neutral v2))

readBackValue :: Value -> Interpreter Expr
readBackValue = \case
  VBase ->
    pure EBase
  VLam f -> do
    var <- newVar
    body <- f (VNeutral (NVar var))
    ELam var <$> readBackValue body
  VNeutral neutral ->
    readBackNeutral neutral

readBackNeutral :: Neutral -> Interpreter Expr
readBackNeutral = \case
  NVar var ->
    pure (EVar var)
  NApp neutral value -> do
    rf <- readBackNeutral neutral
    ra <- readBackValue value
    pure (EApp rf ra)




