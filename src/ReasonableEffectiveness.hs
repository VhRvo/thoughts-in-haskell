module ReasonableEffectiveness where

import Control.Applicative ((<|>))
import Control.Monad (replicateM, when)
import Data.Foldable (for_)
import Data.List.NonEmpty (NonEmpty)

example1 :: Int -> Bool
example1 x = 10 < timesThree x
  where
    timesThree :: Int -> Int
    timesThree = (* 3)

example2 :: Int -> Bool
example2 x = timesThree x
  where
    timesThree :: Int -> Bool
    timesThree y = 10 < (3 * y)

example3 :: Int -> Bool
example3 x = timesThree x (\y -> 10 < y)
  where
    timesThree :: Int -> (Int -> Bool) -> Bool
    timesThree y k = k (3 * y)

example4 :: Int -> Bool
example4 x = timesThree x (\y -> 10 < y)
  where
    timesThree :: Int -> (Int -> r) -> r
    timesThree y k = k (3 * y)

example5 :: Int -> Bool
example5 x =
  timesThree
    x
    ( \y ->
        greaterThanTen
          y
          ( \z ->
              z
          )
    )
  where
    timesThree :: Int -> (Int -> r) -> r
    timesThree y k = k (3 * y)
    greaterThanTen :: Int -> (Bool -> r) -> r
    greaterThanTen y k = k (10 < y)

newtype Cont r a = Cont ((a -> r) -> r)

-- Eliminate Cont
runCont :: Cont r a -> (a -> r) -> r
runCont (Cont m) = m

-- Use the identity continuation to extract the final result.
evalCont :: Cont a a -> a
evalCont m = runCont m id

-- evalCont (Cont m) = m id

pureCont :: a -> Cont r a
pureCont x = Cont (\f -> f x)

bindCont :: Cont r a -> (a -> Cont r b) -> Cont r b
bindCont (Cont mx) f = Cont (\k -> mx (\x -> runCont (f x) k))

instance Functor (Cont r) where
  fmap :: (a -> b) -> Cont r a -> Cont r b
  fmap f (Cont m) = Cont (\k -> m (k . f))

-- fmap f (Cont m) = Cont (\k -> m (\x -> k (f x)))

instance Applicative (Cont r) where
  pure :: a -> Cont r a
  pure = pureCont

  (<*>) :: Cont r (a -> b) -> Cont r a -> Cont r b
  (<*>) (Cont mf) mx = Cont (\k -> mf (\f -> runCont (fmap f mx) k))

-- (<*>) (Cont mf) (Cont mx) = Cont (\k -> mf (\f -> mx (k . f)))
-- (<*>) (Cont mf) (Cont mx) = Cont (\k -> mf (\f -> mx (\x -> k (f x))))

instance Monad (Cont r) where
  (>>=) :: Cont r a -> (a -> Cont r b) -> Cont r b
  (>>=) = bindCont

example6 :: Int -> Bool
example6 x = evalCont $ do
  y <- timesThree x
  z <- greaterThanTen y
  pure z
  where
    timesThree :: Int -> Cont r Int
    timesThree y = Cont (\k -> k y)
    greaterThanTen :: Int -> Cont r Bool
    greaterThanTen y = Cont (\k -> k (10 < y))

-- forall r. (a -> r) -> r
type Done a = forall r. Cont r a

runDone :: Done a -> a
runDone (Cont m) = m id

-- forall r. (a -> Maybe r) -> Maybe r
type Abortable a = forall r. Cont (Maybe r) a

-- fromAbortable :: Abortable a -> Maybe a
-- toAbortable :: Maybe a -> Abortable a

abort :: Abortable x
abort = Cont (\_ -> Nothing)

runAbortable :: Abortable a -> Maybe a
runAbortable (Cont m) = m Just

toAbortable :: Maybe a -> Abortable a
toAbortable = \case
  Nothing -> abort
  Just x -> pure x

secondGuess :: Abortable Bool
secondGuess = Cont (\k -> k True <|> k False)

pureTrue :: Abortable Bool
pureTrue = pure True

-- forall r. (a -> Either e r) -> Either e r
type Except e a = forall r. Cont (Either e r) a

throw :: e -> Except e a
throw e = Cont (\_ -> Left e)

runExcept :: Except e a -> Either e a
runExcept (Cont m) = m Right

toExcept :: Either e a -> Except e a
toExcept = \case
  Left e -> throw e
  Right x -> pure x

-- forall r. (a -> s -> r) -> s -> r
type State s a = forall r. Cont (s -> r) a

get :: State s s
get = Cont (\k s -> k s s)

put :: s -> State s ()
put s = Cont (\k _ -> k () s)

runState :: forall s a. State s a -> s -> (a, s)
runState (Cont m) = m (\a s -> (a, s))

-- forall r. (a -> (w, r)) -> (w, r)
type Writer w a = forall r. Cont (w, r) a

tell :: (Monoid w) => w -> Writer w ()
tell w =
  Cont (\k -> let (w0, r) = k () in (w0 <> w, r))

runWriter :: Monoid w => Writer w a -> (w, a)
runWriter (Cont m) = m (\a -> (mempty, a))

-- forall r. (a -> (s, r)) -> (s, r)
type RState s a = forall r. Cont (s, r) a

fModify :: (s -> s) -> RState s ()
fModify f = Cont (\k -> let (s, r) = k () in (f s, r))

rGet :: RState s s
rGet = Cont (\k -> let (s, r) = k s in (s, r))

runRState :: RState s a -> s -> (s, a)
runRState (Cont m) s = m (\a -> (s, a))

-- forall r. (a -> fw -> (bw, r)) -> fw -> (bw, r)
type Tradis bw fw a = forall r. Cont (fw -> (bw, r)) a


-- forall r. (a -> [r]) -> [r]
type List a = forall r. Cont [r] a

decide :: List Bool
decide = Cont (\k -> k True <> k False)

vanish :: forall a. List a
vanish = Cont (\_ -> [])

runList :: List a -> [a]
runList (Cont m) = m pure

type List1 a = forall r. Cont (NonEmpty r) a
type List' a = forall r. Monoid r => Cont r a
type List1' a = forall r. Semigroup r => Cont r a
-- type Tree0 a = forall r. Cont (Tree r) a

-- (a -> m r) -> m r
type ContT r m a = Cont (m r) a

-- lift :: Monad m => m a -> (a -> m r) -> m r
lift :: Monad m => m a -> ContT r m a
lift mx = Cont (\k -> mx >>= (\x -> k x))

-- forall r. (a -> m r) -> m r
type CodensityT m a = forall r. Cont (m r) a

-- (a -> m ()) -> m ()
type ListT m a = Cont (m ()) a

decideM :: Applicative m => ListT m Bool
decideM = Cont (\k -> k True *> k False)

vanishM :: Applicative m => ListT m a
vanishM = Cont (\_ -> pure ())

runListT :: Applicative m => (a -> m ()) -> ListT m a -> m ()
runListT k (Cont m) = m k

-- All 3 bit patterns
threeBit :: IO ()
threeBit =
  for_ [0, 1] $ \i ->
    for_ [0, 1] $ \j ->
      for_ [0, 1] $ \k ->
        printDigits [i, j, k]

printDigits :: [Int] -> IO ()
printDigits ds = do
  for_ ds (\i -> putStr (show i))
  putStrLn ""

threeBit' :: IO ()
threeBit' = runListT printDigits $ do
  i <- Cont $ for_ [0, 1]
  j <- Cont $ for_ [0, 1]
  k <- Cont $ for_ [0, 1]
  pure [i, j, k]

eightBit :: IO ()
eightBit = runListT printDigits $
  replicateM 8 (Cont (for_ [0, 1]))

-- show only the suffix that changed at every step
eightBit' :: IO ()
eightBit' = runListT pure $ do
  for_ [0..7] $ \n -> do
    i <- Cont $ for_ [0, 1]
    lift $ when (i == 1) $ putStr (replicate n ' ')
    lift $ putStr (show @Int i)
  lift $ putStrLn ""




