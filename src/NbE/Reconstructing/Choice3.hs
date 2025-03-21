module NbE.Reconstructing.Choice3 where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Reader qualified as R
import Control.Monad.State
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.Text (Text)
import Data.Text qualified as T

type Variable = Text

data Type
  = TBase Text
  | TArrow Type Type

data Expr
  = EBase
  | EVar Variable
  | ELam Text Expr
  | EApp Expr Expr

data Value
  = VBase
  | VLam Env Variable Expr
  | VReflect Type Neutral

data Neutral
  = NVar Text
  | NApp Neutral Normal
  deriving (Show)

data Normal
  = NBase
  | NLam Text Normal
  | NNeutral Neutral
  deriving (Show)

type Env = Map Variable Value

newtype Interpreter a
  = Interpreter (ReaderT Env (StateT Int (Either Text)) a)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadState Int,
      MonadError Text
    )

runInterpreter :: Interpreter a -> Env -> Int -> Either Text a
runInterpreter (Interpreter interpreter) env state =
  (`evalStateT` state)
    . (`runReaderT` env)
    $ interpreter

newVar :: Interpreter Variable
newVar = do
  index <- get
  modify' (+ 1)
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
    pure $ VLam env var body
  EApp e1 e2 -> do
    vf <- eval e1
    va <- eval e2
    doApp vf va

doApp :: Value -> Value -> Interpreter Value
doApp v1 v2 = case v1 of
  VLam env param body ->
    R.local (const $ Map.insert param v2 env) (eval body)
  VReflect (TArrow lhs rhs) neutral -> do
    result <- reify lhs v2
    pure $ VReflect rhs (NApp neutral result)
  _ ->
    throwError "base in application target: implementation bug"

reify :: Type -> Value -> Interpreter Normal
reify tpe value = case (tpe, value) of
  (TBase _, VBase) ->
    pure NBase
  (TBase _, VReflect _ neutral) ->
    pure (NNeutral neutral)
  (TArrow lhs rhs, f) -> do
    var <- newVar
    body <- doApp f (VReflect lhs (NVar var))
    NLam var <$> reify rhs body
  _ ->
    error "type mismatched in reify: implementation bug"

-- reflect :: Type -> Neutral -> Interpreter Value
-- reflect tpe neutral = case tpe of
--   TBase _ -> pure (VNeutral neutral)
--   TArrow lhs rhs -> do
--     pure (VLam (\input -> do
--       ra <- reify lhs input
--       reflect rhs (NApp neutral ra)))

test :: IO ()
test = do
  let
    t1 = TArrow (TBase "") (TBase "")
    t2 = TArrow t1 t1
    t3 = TArrow t2 t2
    expr = ELam "hof" (ELam "f" (EApp (EVar "hof") (EVar "f")))
    reified = reify t3 =<< eval expr
    result = runInterpreter reified Map.empty 0
  print result
  pure ()
