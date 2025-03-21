module NbE.Reconstructing.Choice2 where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Reader
import qualified Control.Monad.Reader as R
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
  | VLam (Value -> Interpreter Value)
  | VNeutral Neutral

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
  (`evalStateT` state) .
  (`runReaderT` env) $
  interpreter

newVar :: Interpreter Variable
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
    funV <- eval e1
    argV <- eval e2
    doApp funV argV

doApp :: Value -> Value -> Interpreter Value
doApp v1 v2 = case v1 of
  VNeutral _ ->
    error "neutral expression in application target: implementation bug"
  --   pure (VNeutral (NApp neutral v2))
  VLam f ->
    f v2
  _ ->
    throwError "base in application target: implementation bug"

reify :: Type -> Value -> Interpreter Normal
reify tpe value = case (tpe, value) of
  (TBase _, VBase) ->
    pure NBase
  (TBase _, VNeutral neutral) ->
    pure (NNeutral neutral)
  (TArrow argT resT, VLam f) -> do
    var <- newVar
    var' <- reflect argT (NVar var)
    body <- f var'
    NLam var <$> reify resT body
  _ ->
    -- throwError "type mismatched in reify"
    error "type mismatched in reify: implementation bug"

reflect :: Type -> Neutral -> Interpreter Value
reflect tpe neutral = case tpe of
  TBase _ -> pure (VNeutral neutral)
  TArrow argT resT -> do
    pure (VLam (\input -> do
      ra <- reify argT input
      reflect resT (NApp neutral ra)))

test :: IO ()
test = do
  let t1 = TArrow (TBase "") (TBase "")
      t2 = TArrow t1 t1
      t3 = TArrow t2 t2
      expr = ELam "hof" (ELam "f" (EApp (EVar "hof") (EVar "f")))
      reified = reify t3 =<< eval expr
      result = runInterpreter reified Map.empty 0
  print result
  pure ()
