module WaysToReportErrors where

import qualified Control.Exception as E
import Control.Monad.Except

myDiv1 :: Float -> Float -> Float
myDiv1 x 0 = error "Division by zero"
myDiv1 x y = x / y

example1 :: Float -> Float -> IO ()
example1 x y =
  E.catch @E.SomeException (print (myDiv1 x y))
          print

myDiv2 :: Float -> Float -> Maybe Float
myDiv2 x 0 = Nothing
myDiv2 x y = Just (x / y)

example2 :: Float -> Float -> IO ()
example2 x y =
  case myDiv2 x y of
    Nothing -> putStrLn "Division by zero"
    Just q  -> print q

divSum2 :: Float -> Float -> Float ->
           Maybe Float
divSum2 x y z = do
  xdy <- myDiv2 x y
  xdz <- myDiv2 x z
  pure (xdy + xdz)

myDiv3 :: Float -> Float ->
          Either String Float
myDiv3 x 0 = Left "Division by zero"
myDiv3 x y = Right (x / y)

example3 :: Float -> Float -> IO ()
example3 x y =
  case myDiv3 x y of
    Left msg -> putStrLn msg
    Right q  -> print q

divSum3 :: Float -> Float -> Float ->
           Either String Float
divSum3 x y z = do
  xdy <- myDiv3 x y
  xdz <- myDiv3 x z
  pure (xdy + xdz)

myDiv4 :: (MonadFail m) => Float -> Float ->
          m Float
myDiv4 x 0 = fail "Division by zero"
myDiv4 x y = pure (x / y)

example4a :: Float -> Float -> IO ()
example4a x y =
  case myDiv4 x y of
    Nothing -> putStrLn "Division by zero"
    Just q  -> print q

-- example4b :: Float -> Float -> IO ()
-- example4b x y =
--   case myDiv4 x y of
--     Left msg -> putStrLn msg
--     Right q  -> print q

example4c :: Float -> Float -> IO ()
example4c x y =
  E.catch @E.SomeException (do
    q <- myDiv4 x y
    print q)
    print


data CustomError = DivByZero
                 | OutOfCheese
                 | MiscError String

instance Show CustomError where
  show DivByZero = "Division by zero"
  show OutOfCheese = "Out of cheese"
  show (MiscError str) = str

-- instance Error CustomError where
--   noMsg = MiscError "Unknown error"
--   strMsg str = MiscError str

-- myDiv5 :: (MonadError CustomError m) =>
--           Float -> Float -> m Float
-- myDiv5 x 0 = throwError DivByZero
-- myDiv5 x y = pure (x / y)

-- example5 :: Float -> Float ->
--             Either CustomError String
-- example5 x y =
--   catchError (do q <- myDiv5 x y
--                  pure (show q))
--              (pure . show)