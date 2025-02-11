{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TemplateHaskell #-}

module ReaderTrick.Extended where

import Control.Monad.Reader
import Data.Foldable
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Map ((!))
import Data.Map qualified as Map
import Data.Monoid
import Data.Text (Text)
import Data.Text qualified as Text

newtype Identifier = Identifier Text
  deriving (Eq, Ord, Show)

data Expr
  = Literal Int
  | Variable Identifier
  | Block [Stmt] Expr
  deriving (Eq, Ord, Show)

data Stmt
  = Define Identifier Expr
  | Expr Expr
  deriving (Eq, Ord, Show)

type Env = Map.Map Identifier Int

newtype EvalM a
  = EvalM
  { unEvalM :: Reader Env a
  }
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env
    )

makeBaseFunctor ''Expr
makeBaseFunctor ''Stmt

evaluate :: Expr -> EvalM Int
evaluate = \case
  Literal literal -> pure literal
  Variable identifier -> asks (! identifier)
  Block stmts expr -> do
    executeBlock stmts expr

executeBlock :: [Stmt] -> Expr -> EvalM Int
executeBlock stmts lastExpr =
  appEndo (foldMap execute stmts) (evaluate lastExpr)

execute :: Stmt -> Endo (EvalM a)
execute stmt =
  Endo
    ( \rest -> case stmt of
        Define identifier expr -> do
          value <- evaluate expr
          local (Map.insert identifier value) rest
        Expr expr -> do
          _ <- evaluate expr
          rest
    )

-- executeBlock stmts' last = case stmts' of
--   [] -> evaluate last
--   stmt : stmts -> execute stmt (executeBlock stmts last)

executeBlock' :: [Stmt] -> Expr -> EvalM Int
executeBlock' stmts lastExpr =
  foldl' (\acc stmt -> acc . appEndo (execute stmt)) id stmts (evaluate lastExpr)

-- execute' :: EvalM a -> Stmt -> EvalM a
-- execute' rest stmt = case stmt of
--   Define identifier expr -> do
--     value <- evaluate expr
--     local (Map.insert identifier value) rest
