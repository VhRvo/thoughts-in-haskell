{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module ReaderTrick.Normal where

import Control.Monad.Except
import Control.Monad.Reader
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Maybe (fromMaybe)

data Term
  = Variable Text
  | Abstraction Text Type Term
  | Application Term Term
  deriving (Eq, Ord, Show)

data Type
  = Type :-> Type
  | UnitT
  | BoolT
  deriving (Eq, Ord, Show)

type Context = Map.Map Text Type

data TypeError = TypeError
  deriving (Eq, Show)

newtype TypeCheckM a
  = TypeCheckM
  { unTypeCheckM :: ExceptT TypeError (Reader Context) a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader Context,
      MonadError TypeError
    )

typeCheck :: Term -> TypeCheckM Type
typeCheck = \case
  Variable name -> do
    tpe <- asks . Map.lookup $ name
    maybe (throwError TypeError) pure tpe
  -- context <- ask
  -- case Map.lookup name context of
  --     Just tpe -> pure tpe
  --     Nothing -> throwError $ TypeError
  Abstraction name input body -> do
    output <- local (Map.insert name input) (typeCheck body)
    pure (input :-> output)
  Application term1 term2 -> do
    funcType <- typeCheck term1
    case funcType of
      from :-> to -> do
        from' <- typeCheck term2
        if from == from'
          then pure to
          else throwError TypeError
      _ -> throwError TypeError

data Expr
  = Literal Int
  | Var Text
  | Let Text Expr Expr

type Bindings = Map.Map Text Int

newtype EvalM a
  = EvalM
  { unEvalM :: Reader Bindings a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader Bindings
    )

eval :: Expr -> EvalM Int
eval = \case
    Literal int -> pure int
    Var name -> asks (fromMaybe 0 . Map.lookup name)
    Let name expr body -> do
        value <- eval expr
        local (Map.insert name value) (eval body)


