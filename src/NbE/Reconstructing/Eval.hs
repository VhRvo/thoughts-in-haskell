module NbE.Reconstructing.Eval where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Reader qualified as R
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

type Env = Map Text Value

newtype Interpreter a
  = Interpreter {runInterpreter :: ReaderT Env (Either Text) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadError Text
    )

eval :: Expr -> Interpreter Value
eval = \case
  EBase -> pure VBase
  EVar var -> do
    env <- ask
    liftEither (maybe (Left "variable not found") Right . Map.lookup var $ env)
  ELam var body -> do
    env <- ask
    pure $ VLam (\arg -> local (const (Map.insert var arg env)) (eval body))
  EApp e1 e2 -> do
    eval e1 >>= \case
      VLam f -> do
        v <- eval e2
        f v
      _ -> throwError "not a function in application"
