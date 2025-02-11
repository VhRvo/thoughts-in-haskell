{-# LANGUAGE DeriveAnyClass #-}

module CallStack where

import GHC.Exception
-- import GHC.Stack
import GHC.Stack

giveCallStack :: (HasCallStack) => IO ()
giveCallStack = putStrLn . prettyCallStack . withFrozenCallStack $ callStack

-- giveCallStack = print callStack

getSrcLoc :: (HasCallStack) => SrcLoc
getSrcLoc = snd $ head $ getCallStack callStack

giveCallStackArbitraryName :: (HasCallStack) => IO ()
giveCallStackArbitraryName = print callStack

demo :: IO ()
demo = do
  --   print getSrcLoc
  giveCallStack
  print @Int 11234

main :: (HasCallStack) => IO ()
main = do
  -- giveCallStackArbitraryName
  giveCallStack
  --   print callStack
  putStrLn . prettyCallStack $ callStack
  putStrLn . prettyCallStack . withFrozenCallStack $ callStack
  --   print getSrcLoc
  demo

-- foo :: HasCallStack => [a] -> a
-- foo [] = error "impossible!"
-- foo (a:_) = a

-- bar :: HasCallStack => [Int] -> [Int] -> Int
-- bar a b = foo a + foo b

data FooException = (HasCallStack) => FooException
  deriving anyclass (Exception)

instance Show FooException where
  show :: FooException -> String
  show FooException = "FooException\n" <> prettyCallStack callStack

foo :: (HasCallStack) => [a] -> a
foo [] = throw FooException
foo (a : _) = a

bar :: (HasCallStack) => [Int] -> [Int] -> Int
bar a b = foo a + foo b
