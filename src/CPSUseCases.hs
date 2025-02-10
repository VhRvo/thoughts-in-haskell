module CPSUseCases where

import Control.Monad (unless, when)
import Control.Monad.Cont
import Control.Monad.Trans
import qualified System.Random as R

-- import Data.Map.Strict (Map, fromList, (!?))
-- import qualified Data.Map.Strict as Map
-- import Data.Text
-- import Test.Hspec.Expectations
-- import Test.QuickCheck
-- import Prelude hiding (fail, succ)

fastCPS :: Integer -> (Integer -> r) -> r
fastCPS 0 k = k 0
fastCPS n k = fastCPS (n - 1) (\x -> x `seq` k (x * n))

data Tree
  = Leaf Int
  | Branch Tree Tree

leafSumCPS :: Tree -> (Int -> r) -> r
leafSumCPS tree k = case tree of
  Leaf i -> k i
  Branch left right -> leafSumCPS left (\left' -> leafSumCPS right (\right' -> k (left' + right')))

leafSumCPS' :: forall r. Tree -> (Int -> r) -> r
leafSumCPS' tree' k' = go tree' k'
  where
    go :: Tree -> (Int -> r) -> r
    go tree k = case tree of
      Leaf i -> if i == 6 then k' 1000 else k i
      Branch left right ->
        leafSumCPS
          left
          ( \left' ->
              leafSumCPS
                right
                ( \right' ->
                    k (left' + right')
                )
          )

leafSumCont :: forall r. Tree -> Cont r Int
leafSumCont tree = callCC $ \k ->
  let go :: Tree -> Cont r Int
      go = \case
        Leaf i -> if i == 6 then k 1000 else pure i
        Branch left right -> do
          left' <- go left
          right' <- go right
          pure (left' + right')
   in go tree

forLoop :: (Monad m) => [a] -> (a -> ContT () m c) -> m ()
forLoop items function =
  let contArray = function <$> items
   in runContT (sequence_ contArray) pure

breakOut :: (Monad m) => ContT () m c
breakOut = ContT (\_ -> pure ())

breakOutIf :: (Monad m) => Bool -> ContT () m ()
breakOutIf = \case
  True -> ContT $ \_ -> pure ()
  False -> pure ()

infiniteLoop :: IO ()
infiniteLoop = forLoop @_ @Int [1 ..] $ \index -> do
  if index > 10
    then breakOut
    else lift $ print index

infiniteLoop2 :: IO ()
infiniteLoop2 = forLoop @_ @Int [1 ..] $ \index -> do
  breakOutIf (index > 10)
  lift $ print index

goto :: ContT r m (ContT r m b)
goto = callCC $ \k -> let fn = k fn in pure fn

-- we can also provide back other arguments,
-- in this case some number,
-- to allow more intelligent looping:
gotoC :: ContT r m (Int -> ContT r m b, Int)
gotoC = callCC $ \k -> let fn num = k (fn, num) in pure (fn, 0)

-- based on the output of a random number generator,
-- we either go back to marker1, marker2, or finish

-- >>> gotoEx1
gotoEx1 :: IO ()
gotoEx1 = (`runContT` pure) $ do
  marker1 <- goto
  lift $ putStrLn "one"

  marker2 <- goto
  lift $ putStrLn "two"

  num <- lift $ R.randomRIO @Int (0, 2)
  if num < 1
    then marker1
    else
      if num < 2
        then marker2
        else lift $ putStrLn "done"

-- >>> gotoEx2
gotoEx2 :: IO ()
gotoEx2 = (`runContT` pure) $ do
  (marker1, num) <- gotoC
  lift (putStrLn ("count: " <> show num))
  if num < 10
    then marker1 (num + 1)
    else lift (putStrLn "done")

makeList :: Cont [a] a -> [a]
makeList = (`runCont` (: []))

each :: (Monad m) => m a -> Cont (m b) a
each array = cont $ \k -> array >>= k

ignoreIf :: Bool -> Cont [a] ()
ignoreIf b = cont $ \k ->
  if b then [] else k ()

eachEx1 :: [(Int, Int, Int)]
eachEx1 = makeList $ do
  n1 <- each [1, 2, 3]
  n2 <- each [4, 5, 6]
  n3 <- each [7, 8, 9]
  ignoreIf (n3 == 8)
  pure (n1, n2, n3)

-- >>> eachEx1
-- [(1,4,7),(1,4,9),(1,5,7),(1,5,9),(1,6,7),(1,6,9),(2,4,7),(2,4,9),(2,5,7),(2,5,9),(2,6,7),(2,6,9),(3,4,7),(3,4,9),(3,5,7),(3,5,9),(3,6,7),(3,6,9)]

listEx1 :: [(Int, Int, Int)]
listEx1 = do
  n1 <- [1, 2, 3]
  n2 <- [4, 5, 6]
  n3 <- [7, 8, 9]
  if n3 == 8
    then []
    else pure (n1, n2, n3)

-- >>> listEx1
-- [(1,4,7),(1,4,9),(1,5,7),(1,5,9),(1,6,7),(1,6,9),(2,4,7),(2,4,9),(2,5,7),(2,5,9),(2,6,7),(2,6,9),(3,4,7),(3,4,9),(3,5,7),(3,5,9),(3,6,7),(3,6,9)]

divException :: Int -> Int -> (String -> Cont r Int) -> Cont r Int
divException x y handler = callCC $ \ok -> do
  err <- callCC $ \notOk -> do
    when (y == 0) (notOk "Denominator 0")
    ok $ x `div` y
  handler err

-- >>> runCont (divException 10 2 error) id
-- >>> runCont (divException 10 0 error) id
-- 5
-- Denominator 0

tryCont :: MonadCont m => ((err -> m a) -> m a) -> (err -> m a) -> m a
tryCont handlerCont handler = callCC $ \ok -> do
  err <- callCC $ \notOk -> do
    x <- handlerCont notOk
    ok x
  handler err

data SqrtException = LessThanZero
  deriving (Show, Eq)

sqrtIO :: (SqrtException -> ContT r IO ()) -> ContT r IO ()
sqrtIO throw = do
  line <- lift (putStr "Enter a number to sqrt: " >> readLn @Float)
  when (line < 0) (throw LessThanZero)
  lift $ print (sqrt line)

test :: IO ()
test = runContT (tryCont sqrtIO (lift . print)) pure

-- >>> test
-- user error (Prelude.readIO: no parse)

