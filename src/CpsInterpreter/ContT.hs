module CpsInterpreter.ContT where

import Control.Arrow
import Control.Monad.Base
import Control.Monad.Except (ExceptT, MonadError (..), runExceptT)
import Control.Monad.IO.Class
import Control.Monad.Cont (ContT, runContT, MonadCont (..))
import Control.Monad.Reader (MonadReader (..), ReaderT, runReaderT)
import qualified Control.Monad.Reader as Reader
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import Data.Text (Text)
import Text.Pretty.Simple (pPrint)

data Expr
  = Variable Int
  | Lambda Expr
  | Application Expr Expr
  | Literal Int
  | Prim PrimOp Expr Expr
  | Escape Expr
  deriving (Eq, Ord, Show)

data PrimOp
  = Add
  | Mul
  deriving (Eq, Ord, Show)

-- type Continuation = Value -> Interpreter Value

data Value
  = IntegerV Int
  | ClosureV (Value -> Interpreter Value Value)

instance Show Value where
  show :: Value -> String
  show = \case
    IntegerV number -> show number
    ClosureV _ -> "<<closure>>"

type Env = Seq Value

newtype Interpreter r a = Interpreter
  {unInterpreter :: ReaderT Env (ExceptT Text (ContT (Either Text r) IO)) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader Env,
      MonadError Text,
      MonadCont,
      MonadIO,
      MonadBase IO
    )

eval :: Expr -> Interpreter Value Value
eval expr = case expr of
  Variable index -> do
    env <- Reader.ask
    case Seq.lookup index env of
      Nothing -> throwError "variable not found"
      Just value -> pure value
  Lambda body ->
    pure $ ClosureV $ \value ->
      Reader.local (value <|) (eval body)
  Application functionE argumentE ->
    eval functionE >>= \case
      IntegerV _ ->
        throwError "integer cannot be function"
      ClosureV closure -> do
        eval argumentE >>= closure
  Literal integer -> pure $ IntegerV integer
  Prim op leftE rightE ->
    eval leftE >>= \case
      IntegerV left ->
        eval rightE >>= \case
          IntegerV right -> case op of
            Add -> pure $ IntegerV $ left + right
            Mul -> pure $ IntegerV $ left * right
          _ -> throwError "function cannot be operated"
      _ -> throwError "function cannot be operated"
  Escape body -> do
    callCC $ \k ->
      Reader.local (ClosureV k <|) (eval body)

evalExpr :: Expr -> Interpreter Value Value
evalExpr = eval

runInterpreter :: Interpreter Value Value -> IO (Either Text Value)
runInterpreter = (`runContT` pure) . runExceptT . (`runReaderT` Seq.empty) . unInterpreter

demo :: IO ()
demo = do
  value <- runInterpreter $
      evalExpr $
        Escape (Prim Mul (Prim Add (Literal 1) (Literal 3)) (Prim Add (Literal 4) (Application (Variable 0) (Literal 10))))
  pPrint value
  -- pure ()
