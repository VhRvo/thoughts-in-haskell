module Exception where

import Control.Exception hiding (AssertionFailed (..))
import Data.Text
import Data.Typeable

-- data SomeException'
--     = forall e. (Show e, Exception e) => SomeException' e

e1 = DivideByZero

newtype AssertionFailed = AssertionFailed String
  deriving (Typeable, Show)

instance Exception AssertionFailed

main :: IO ()
main = do
  throw (AssertionFailed "foo")
    `catch` \(e :: AssertionFailed) -> print e

main2 :: IO ()
main2 = do
  print (fromException @ArithException (toException DivideByZero))
  print (fromException @SomeException (toException DivideByZero))

main1 :: IO ()
main1 = do
  --   throw DivideByZero `catch` (\(e :: ArithException) -> print e)
  --   throw DivideByZero `catch` (\(e :: ArrayException) -> print e)
  throw DivideByZero
    `catch` (\(e :: IOException) -> print "caught IO")
    `catch` (\(e :: SomeException) -> print "other")
  throw DivideByZero
    `catch` (\(e :: ArithException) -> print "caught Arith")
    `catch` (\(e :: SomeException) -> print "other")
