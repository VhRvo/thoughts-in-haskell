{-# LANGUAGE InstanceSigs #-}

module Transformers where

import Control.Monad.Except
  ( ExceptT (..),
    MonadError (throwError),
    runExceptT,
  )
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader
  ( MonadReader (ask, local),
    ReaderT (..),
    asks,
  )
import Control.Monad.State.Strict
  ( MonadState,
    StateT (runStateT),
    modify,
  )
import Control.Monad.Writer.Strict
  ( MonadWriter (tell),
    WriterT (runWriterT),
  )
import Data.Map.Strict qualified as Map
import Data.Maybe (fromJust)
import Data.Text (Text)

type Name = Text

data Expr
  = Literal Int
  | Variable Name
  | Plus Expr Expr
  | Abstraction Name Expr
  | Application Expr Expr
  deriving (Eq, Ord, Show)

type Env = Map.Map Name Value

data Value
  = IntegerV Int
  | FunctionV Env Name Expr
  deriving (Show)

eval0 :: Env -> Expr -> Value
eval0 env = \case
  Literal i -> IntegerV i
  Variable name -> fromJust (Map.lookup name env)
  Plus e1 e2 ->
    let
      IntegerV i1 = eval0 env e1
      IntegerV i2 = eval0 env e2
     in
      IntegerV (i1 + i2)
  Abstraction name body -> FunctionV env name body
  Application e1 e2 ->
    let
      val1 = eval0 env e1
      val2 = eval0 env e2
     in
      case val1 of
        FunctionV env' name body -> eval0 (Map.insert name val2 env') body

exampleExpr :: Expr
exampleExpr = Literal 12 `Plus` Application (Abstraction "x" (Variable "x")) (Literal 4 `Plus` Literal 2)

-- >>> eval0 Map.empty exampleExpr
-- IntegerV 18

newtype Eval1 a = Eval1 {runEval1 :: Identity a}
  deriving newtype (Functor, Applicative, Monad)

instance MonadFail Eval1 where
  fail :: String -> Eval1 a
  fail = error

-- eval1 :: Env -> Expr -> Eval1 Value
eval1 :: (Monad m, MonadFail m) => Env -> Expr -> m Value
eval1 env = \case
  Literal i -> pure $ IntegerV i
  Variable name ->
    -- Map.lookup name env
    pure $ fromJust (Map.lookup name env)
  Plus e1 e2 -> do
    IntegerV i1 <- eval1 env e1
    IntegerV i2 <- eval1 env e2
    pure $ IntegerV (i1 + i2)
  Abstraction name body ->
    pure $ FunctionV env name body
  Application e1 e2 -> do
    FunctionV env' name body <- eval1 env e1
    val2 <- eval1 env e2
    eval1 (Map.insert name val2 env') body

-- >>> runIdentity . runEval1 $ eval1 Map.empty exampleExpr
-- IntegerV 18

newtype Eval2 a = Eval2 {runEval2 :: ExceptT Text Identity a}
  deriving newtype (Functor, Applicative, Monad, MonadError Text)

runEval2' :: Eval2 Value -> Either Text Value
runEval2' = runIdentity . runExceptT . runEval2

-- instance MonadFail Eval2 where
--     fail :: String -> Eval2 a
--     fail = error

eval2 :: Env -> Expr -> Eval2 Value
eval2 env = \case
  Literal i -> pure $ IntegerV i
  Variable name -> case Map.lookup name env of
    Nothing -> throwError $ "Variable not found: " <> name
    Just value -> pure value
  Plus e1 e2 -> do
    e1' <- eval2 env e1
    e2' <- eval2 env e2
    case (e1', e2') of
      (IntegerV i1, IntegerV i2) -> pure $ IntegerV (i1 + i2)
      _ -> throwError "type error in plus"
  Abstraction name body -> do
    pure $ FunctionV env name body
  Application e1 e2 -> do
    func <- eval2 env e1
    case func of
      FunctionV env' name body -> do
        arg <- eval2 env e2
        eval2 (Map.insert name arg env') body
      _ -> throwError "type error in application"

-- >>> runEval2' $ eval2 Map.empty exampleExpr
-- Right (IntegerV 18)
-- >>> runEval2' $ eval2 Map.empty (Plus (Literal 1) (Abstraction "x" (Variable "x")))
-- Left "type error in plus"

newtype Eval3 a = Eval3 {runEval3 :: ReaderT Env (ExceptT Text Identity) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadError Text)

runEval3' :: Env -> Eval3 Value -> Either Text Value
runEval3' env = runIdentity . runExceptT . (`runReaderT` env) . runEval3

eval3 :: Expr -> Eval3 Value
eval3 = \case
  Literal i -> pure (IntegerV i)
  Variable name -> do
    env <- ask
    case Map.lookup name env of
      Nothing -> throwError $ "Variable not found: " <> name
      Just value -> pure value
  Plus e1 e2 -> do
    e1' <- eval3 e1
    e2' <- eval3 e2
    case (e1', e2') of
      (IntegerV i1, IntegerV i2) -> pure (IntegerV (i1 + i2))
      _ -> throwError "type error in plus"
  Abstraction name body -> do
    -- env <- ask
    -- pure (FunctionV env name body)
    asks (\env -> FunctionV env name body)
  Application e1 e2 -> do
    e1' <- eval3 e1
    arg <- eval3 e2
    case e1' of
      FunctionV env name body -> local (const (Map.insert name arg env)) (eval3 body)
      _ -> throwError "type error in application"

-- >>> runEval3' Map.empty $ eval3 exampleExpr
-- Right (IntegerV 18)

tick :: (Num s, MonadState s m) => m ()
tick = modify (+ 1)

newtype Eval4 a = Eval4 {runEval4 :: ReaderT Env (ExceptT Text (StateT Int Identity)) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadError Text, MonadState Int)

runEval4' :: Env -> Int -> Eval4 Value -> (Either Text Value, Int)
runEval4' env st = runIdentity . (`runStateT` st) . runExceptT . (`runReaderT` env) . runEval4

eval4 :: Expr -> Eval4 Value
eval4 = \case
  Literal i -> do
    tick
    pure (IntegerV i)
  Variable name -> do
    tick
    env <- ask
    case Map.lookup name env of
      Nothing -> throwError ("variable not found: " <> name)
      Just value -> pure value
  Plus e1 e2 -> do
    tick
    e1' <- eval4 e1
    e2' <- eval4 e2
    case (e1', e2') of
      (IntegerV i1, IntegerV i2) -> do
        pure (IntegerV (i1 + i2))
      _ -> throwError "type error in plus"
  Abstraction name body -> do
    tick
    asks (\env -> FunctionV env name body)
  Application e1 e2 -> do
    tick
    e1' <- eval4 e1
    arg <- eval4 e2
    case e1' of
      FunctionV env name body ->
        local (const (Map.insert name arg env)) (eval4 body)
      _ -> throwError "type error in application"

newtype Eval'4 a = Eval'4 {runEval'4 :: ReaderT Env (StateT Int (ExceptT Text Identity)) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadState Int, MonadError Text)

runEval'4' :: Env -> Int -> Eval'4 Value -> Either Text (Value, Int)
runEval'4' env st = runIdentity . runExceptT . (`runStateT` st) . (`runReaderT` env) . runEval'4

newtype Eval5 a = Eval5 {runEval5 :: ReaderT Env (ExceptT Text (WriterT [Text] (StateT Int Identity))) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadError Text, MonadWriter [Text], MonadState Int)

runEval5' :: Env -> Int -> Eval5 Value -> ((Either Text Value, [Text]), Int)
runEval5' env st = runIdentity . (`runStateT` st) . runWriterT . runExceptT . (`runReaderT` env) . runEval5

eval5 :: Expr -> Eval5 Value
eval5 = \case
  Literal i -> do
    tick
    pure (IntegerV i)
  Variable name -> do
    tick
    tell [name]
    env <- ask
    case Map.lookup name env of
      Nothing -> throwError ("variable not found: " <> name)
      Just value -> pure value
  Plus e1 e2 -> do
    tick
    e1' <- eval5 e1
    e2' <- eval5 e2
    case (e1', e2') of
      (IntegerV i1, IntegerV i2) -> do
        pure (IntegerV (i1 + i2))
      _ -> throwError "type error in plus"
  Abstraction name body -> do
    tick
    asks (\env -> FunctionV env name body)
  Application e1 e2 -> do
    tick
    e1' <- eval5 e1
    arg <- eval5 e2
    case e1' of
      FunctionV env name body ->
        local (const (Map.insert name arg env)) (eval5 body)
      _ -> throwError "type error in application"

newtype Eval6 a = Eval6 {runEval6 :: ReaderT Env (ExceptT Text (WriterT [Text] (StateT Int IO))) a}
  deriving newtype (Functor, Applicative, Monad, MonadReader Env, MonadError Text, MonadWriter [Text], MonadState Int, MonadIO)

runEval6' :: Env -> Int -> Eval6 Value -> IO ((Either Text Value, [Text]), Int)
runEval6' env st = (`runStateT` st) . runWriterT . runExceptT . (`runReaderT` env) . runEval6

eval6 :: Expr -> Eval6 Value
eval6 = \case
  Literal i -> do
    tick
    liftIO $ print i
    pure (IntegerV i)
  Variable name -> do
    tick
    tell [name]
    env <- ask
    case Map.lookup name env of
      Nothing -> throwError ("variable not found: " <> name)
      Just value -> pure value
  Plus e1 e2 -> do
    tick
    e1' <- eval6 e1
    e2' <- eval6 e2
    case (e1', e2') of
      (IntegerV i1, IntegerV i2) -> do
        pure (IntegerV (i1 + i2))
      _ -> throwError "type error in plus"
  Abstraction name body -> do
    tick
    asks (\env -> FunctionV env name body)
  Application e1 e2 -> do
    tick
    e1' <- eval6 e1
    arg <- eval6 e2
    case e1' of
      FunctionV env name body ->
        local (const (Map.insert name arg env)) (eval6 body)
      _ -> throwError "type error in application"
