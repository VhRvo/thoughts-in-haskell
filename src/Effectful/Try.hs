module Effectful.Try where

import Data.Kind (Type)
import Data.Text (Text)

import Effectful
import Effectful.Reader.Static

-- type Env = Eff '[Reader Text, State Int, IOE]
type Env :: Type -> Type
type Env = Eff '[Reader Text, Reader Int, IOE]

envDemo :: Env Text
envDemo = do
    env <- ask @Text
    offset <- ask @Int
    liftIO $ print env >> print offset
    pure $ env <> "!"

main :: IO ()
main = do
    result <- runEff . runReader 3 . runReader "Haa" $ envDemo
    -- Error
    -- result <- runEff . runReader "LoL" . runReader 2 $ envDemo
    print result


