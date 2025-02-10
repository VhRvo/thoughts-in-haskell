module StateInReader where

import Control.Monad.IO.Class
import Control.Monad.Base (MonadBase, liftBase)
import Control.Monad.Reader
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef.Lifted

import Prelude hiding (init)

newtype Eval a = Eval {unEval :: ReaderT (IORef (Map Text Int)) IO a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader (IORef (Map Text Int)),
      MonadIO,
      MonadBase IO
    )

eval :: IORef (Map Text Int) -> Eval a -> IO a
eval init = (`runReaderT` init) . unEval

demo1 :: Eval ()
demo1 = do
    envRef <- ask
    env <- readIORef envRef
    liftIO $ print env
    modifyIORef envRef (Map.insert "a" 1)
    env <- readIORef envRef
    liftIO $ print env
    envRef <- ask
    env <- readIORef envRef
    liftIO $ print env

main :: IO ()
main = do
    envRef <- newIORef Map.empty
    eval envRef demo1

