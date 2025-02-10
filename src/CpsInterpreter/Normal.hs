module CpsInterpreter.Normal where

import Control.Monad.Base
import Control.Monad.Except (ExceptT (..), MonadError (..))
import Control.Monad.IO.Class
import Control.Monad.Reader (MonadReader (..), ReaderT (..))
import qualified Control.Monad.Reader as Reader
import Data.Sequence (Seq, (<|))
import qualified Data.Sequence as Seq
import Data.Text (Text)

data Expr
  = Variable Int
  | Lambda Expr
  | Application Expr Expr
  | Literal Int
  | Prim PrimOp Expr Expr
  deriving (Eq, Ord, Show)

data PrimOp
  = Add
  | Mul
  deriving (Eq, Ord, Show)

data Value
  = IntegerV Int
  | ClosureV (Value -> Interpreter Value)

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

eval :: Expr -> Interpreter Value
eval = \case
  Variable index -> do
    env <- Reader.ask
    case Seq.lookup index env of
      Nothing -> throwError "variable not found"
      Just value -> pure value
  Lambda body ->
    pure . ClosureV $ \value ->
      Reader.local (value <|) (eval body)
  Application functionE argumentE ->
    eval functionE >>= \case
      IntegerV _ ->
        throwError "integer cannot be function"
      ClosureV closure -> do
        argument <- eval argumentE
        closure argument
  Literal integer -> pure . IntegerV $ integer
  Prim op leftE rightE ->
    eval leftE >>= \case
      IntegerV left ->
        eval rightE >>= \case
          IntegerV right -> case op of
            Add -> pure . IntegerV $ left + right
            Mul -> pure . IntegerV $ left * right
          _ -> throwError "function cannot be operated"
      _ -> throwError "function cannot be operated"
