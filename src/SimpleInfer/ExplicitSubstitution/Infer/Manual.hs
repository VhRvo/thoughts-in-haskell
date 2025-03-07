{-# LANGUAGE StrictData #-}

module SimpleInfer.ExplicitSubstitution.Infer.Manual where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Reader qualified as Reader
import Control.Monad.State.Strict
import Control.Monad.State.Strict qualified as State
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as T
import SimpleInfer.ExplicitSubstitution.Substitution
import SimpleInfer.Syntax
import SimpleInfer.TypeEnv
import Prelude hiding (id, lookup)
import Prelude qualified as P

newtype Infer r e a
  = Infer {runInfer :: ReaderT r (StateT Int (ExceptT e IO)) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader r, MonadState Int, MonadError e, MonadIO)

fresh :: Infer r e Type
fresh = do
  index <- State.get
  State.modify' (+ 1)
  pure (TVariable (T.pack ('$' : show index)))

infer :: Expr -> Infer TypeEnv Text (Substitution, Type)
infer = \case
  EVariable id -> do
    env <- Reader.ask
    case lookup id env of
      Nothing -> throwError "variable not found"
      Just tpe -> pure (mempty, tpe)
  ELambda id body -> do
    unknown <- fresh
    Reader.local (insert id unknown) (infer body)
  EApplication fun arg -> do
    retT <- fresh
    (s1, funT) <- infer fun
    (s2, argT) <- Reader.local (apply s1) (infer arg)
    s3 <- unify (TArrow argT retT) (apply s2 funT)
    pure (s3 <> s2 <> s1, apply s3 retT)
  ELet id rhs body -> do
    (s1, rhsT) <- infer rhs
    Reader.local (insert id rhsT . apply s1) (infer body)
  ELiteral literal -> do
    case literal of
      LInteger _ -> pure (mempty, TInteger)
      LBoolean _ -> pure (mempty, TBoolean)

unify :: Type -> Type -> Infer TypeEnv Text Substitution
unify = go
  where
    go TBoolean TBoolean = pure mempty
    go TInteger TInteger = pure mempty
    go (TVariable lhs) rhs'@(TVariable rhs)
      | lhs == rhs = pure mempty
      | otherwise = pure (singleton lhs rhs')
    go (TVariable id) tpe =
      bind id tpe
    go tpe (TVariable id) =
      bind id tpe
    go (TArrow argT retT) (TArrow argT' retT') = do
      s1 <- unify argT argT'
      s2 <- unify (apply s1 retT) (apply s1 retT')
      pure (s2 <> s1)
    go _ _ = throwError "unify error"

occurCheck :: Identifier -> Type -> Bool
occurCheck id tpe = Set.member id (freeVars tpe)

bind :: Identifier -> Type -> Infer TypeEnv Text Substitution
bind id tpe
  | occurCheck id tpe = throwError "infinite type"
  | otherwise = pure (singleton id tpe)
