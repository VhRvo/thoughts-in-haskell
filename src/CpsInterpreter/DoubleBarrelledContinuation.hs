module CpsInterpreter.DoubleBarrelledContinuation where

import Control.Monad.Base
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import Control.Monad.Reader qualified as Reader
import Data.Sequence (Seq, (<|))
import Data.Sequence qualified as Seq
import Data.Text (Text)
import Text.Pretty.Simple (pPrint)

data Expr
  = Variable Int
  | Lambda Expr
  | Application Expr Expr
  | Literal Int
  | Prim PrimOp Expr Expr
  | Escape Expr
  | Handle Expr Expr
  | Raise Expr
  deriving (Eq, Ord, Show)

data PrimOp
  = Add
  | Mul
  deriving (Eq, Ord, Show)

type Continuation = Value -> Interpreter Value

data Value
  = IntegerV Int
  | ClosureV (Value -> Continuation -> Interpreter Value)

instance Show Value where
  show :: Value -> String
  show = \case
    IntegerV number -> show number
    ClosureV _ -> "<<closure>>"

type Env = Seq Value

newtype Interpreter a = Interpreter
  {unInterpreter :: ReaderT Env (ExceptT Text IO) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadError Text,
      MonadIO,
      MonadBase IO
    )

eval :: Expr -> Continuation -> Continuation -> Interpreter Value
eval expr handler k = case expr of
  Variable index -> do
    env <- Reader.ask
    case Seq.lookup index env of
      Nothing -> throwError "variable not found"
      Just value -> k value
  Lambda body ->
    k $ ClosureV $ \value k' ->
      Reader.local (value <|) (eval body handler k')
  Application functionE argumentE ->
    eval
      functionE
      handler
      $ \case
        IntegerV _ ->
          throwError "integer cannot be function"
        ClosureV closure -> do
          eval
            argumentE
            handler
            (`closure` k)
  Literal integer -> k $ IntegerV integer
  Prim op leftE rightE ->
    eval leftE handler $ \case
      IntegerV left ->
        eval rightE handler $ \case
          IntegerV right -> case op of
            Add -> k $ IntegerV $ left + right
            Mul -> k $ IntegerV $ left * right
          _ -> throwError "function cannot be operated"
      _ -> throwError "function cannot be operated"
  Escape body ->
    Reader.local
      (\env -> ClosureV (\value _ -> k value) <| env)
      (eval body handler k)
  Handle bodyE handlerE -> do
    eval bodyE (\value -> Reader.local (value <|) (eval handlerE handler k)) k
  Raise errorE -> do
    eval errorE handler handler

evalExpr :: Expr -> Interpreter Value
evalExpr expr = eval expr (\_ -> error "uncaught exception") pure

runInterpreter :: Interpreter a -> IO (Either Text a)
runInterpreter = runExceptT . (`runReaderT` Seq.empty) . unInterpreter

demo :: IO ()
demo = do
  value <-
    runInterpreter $
      evalExpr $
        Prim Add (Literal 1) (Raise (Literal 2)) `Handle` Prim Add (Variable 0) (Literal 3)
  pPrint value
