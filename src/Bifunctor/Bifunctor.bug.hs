{-# LANGUAGE FlexibleInstances #-}
{-# HLINT ignore "Use first" #-}
{-# HLINT ignore "Use second" #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# HLINT ignore "Use sequenceA" #-}
{-# HLINT ignore "Use mapM" #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

module Bifunctor where

-- import Data.Bifunctor
-- import Data.Bitraversable

import Control.Monad.State.Strict (MonadState (..), State, modify', runState, state)
import Data.Char (isSpace)
import Data.Function ((&))
import Data.Semigroup (Max (Max, getMax))
import Prelude hiding (Traversable (..), map)

class Bifunctor s where
  bimap :: (a -> b) -> (c -> d) -> s a c -> s b d
  first :: (a -> b) -> s a c -> s b c
  first f = bimap f id
  second :: (c -> d) -> s a c -> s a d
  second = bimap id

-- class Bitraversable

newtype Fix s a
  = In {out :: s a (Fix s a)}

instance (Bifunctor s) => Functor (Fix s) where
  fmap :: (a -> b) -> Fix s a -> Fix s b
  fmap = map

map :: (Bifunctor s) => (a -> b) -> Fix s a -> Fix s b
map f = In . bimap f (map f) . out

map' :: (Bifunctor s) => (a -> b) -> Fix s a -> Fix s b
map' f = fold (In . first f)

fold :: (Bifunctor s) => (s a b -> b) -> Fix s a -> b
fold f = f . second (fold f) . out

unfold :: (Bifunctor s) => (b -> s a b) -> b -> Fix s a
unfold f = In . second (unfold f) . f

data (m :*: n) a = Prod {pFst :: m a, pSnd :: n a}

(<:*:>) :: (Functor m, Functor n) => (a -> m b) -> (a -> n b) -> a -> (m :*: n) b
(f <:*:> g) x = Prod (f x) (g x)

instance (Functor m, Functor n) => Functor (m :*: n) where
  fmap :: (a -> b) -> (m :*: n) a -> (m :*: n) b
  fmap f (Prod mx nx) = Prod (f <$> mx) (f <$> nx)

instance (Applicative m, Applicative n) => Applicative (m :*: n) where
  pure :: a -> (m :*: n) a
  pure x = Prod (pure x) (pure x)
  (<*>) :: (m :*: n) (a -> b) -> (m :*: n) a -> (m :*: n) b
  Prod mf nf <*> Prod mx nx = Prod (mf <*> mx) (nf <*> nx)

newtype (m :.: n) a = Comp {unComp :: m (n a)}

(<:.:>) :: (Functor m, Functor n) => (b -> n c) -> (a -> m b) -> a -> (m :.: n) c
f <:.:> g = Comp . fmap f . g

instance (Functor m, Functor n) => Functor (m :.: n) where
  fmap :: (a -> b) -> (m :.: n) a -> (m :.: n) b
  fmap f = Comp . fmap (fmap f) . unComp

instance (Applicative m, Applicative n) => Applicative (m :.: n) where
  pure :: a -> (m :.: n) a
  pure = Comp . pure . pure
  (<*>) :: (m :.: n) (a -> b) -> (m :.: n) a -> (m :.: n) b
  Comp mnf <*> Comp mnx = Comp ((<*>) <$> mnf <*> mnx)

class (Functor t) => Traversable t where
  traverse :: (Applicative m) => (a -> m b) -> t a -> m (t b)
  traverse f = sequence . fmap f
  sequence :: (Applicative m) => t (m a) -> m (t a)
  sequence = traverse id

instance Traversable [] where
  sequence :: (Applicative m) => [m a] -> m [a]
  sequence = \case
    [] -> pure []
    x : xs -> (:) <$> x <*> sequence xs

reduce :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
reduce f = unConst . traverse (Const . f)

crush :: (Traversable t, Monoid m) => t m -> m
crush = reduce id

sum :: (Traversable t) => t Int -> Int
sum = getMax . crush . fmap Max

class (Bifunctor s) => Bitraversable s where
  bisequence :: (Applicative f) => s (f a) (f b) -> f (s a b)

instance (Bitraversable s) => Traversable (Fix s) where
  traverse :: (Applicative f) => (a -> f b) -> Fix s a -> f (Fix s b)
  --   traverse f = fmap In . bisequence . bimap f (traverse f) . out
  traverse f = fold (fmap In . bisequence . first f)

class Coerce a b | a -> b where
  reveal :: a -> b
  wrap :: b -> a

instance (Coerce (m a) b, Coerce (n a) c) => Coerce ((m :*: n) a) (b, c) where
  reveal :: (m :*: n) a -> (b, c)
  reveal (Prod mx nx) = (reveal mx, reveal nx)
  wrap :: (b, c) -> (m :*: n) a
  wrap (x, y) = Prod (wrap x) (wrap y)

instance
  (Functor m, Functor n, Coerce (m b) c, Coerce (n a) b) =>
  Coerce ((m :.: n) a) c
  where
  reveal :: (m :.: n) a -> c
  reveal = reveal . fmap reveal . unComp
  wrap :: c -> (m :.: n) a
  wrap = Comp . fmap wrap . wrap

instance Coerce (Maybe a) (Maybe a) where
  reveal :: Maybe a -> Maybe a
  reveal = id
  wrap :: Maybe a -> Maybe a
  wrap = id

instance Coerce (State s a) (s -> (a, s)) where
  reveal :: State s a -> s -> (a, s)
  reveal = runState
  wrap :: (s -> (a, s)) -> State s a
  wrap = state

newtype Id a = Id {unId :: a}

instance Functor Id where
  fmap :: (a -> b) -> Id a -> Id b
  fmap f = Id . f . unId

instance Applicative Id where
  pure :: a -> Id a
  pure = Id
  (<*>) :: Id (a -> b) -> Id a -> Id b
  Id f <*> Id x = Id (f x)

instance Coerce (Id a) a where
  reveal :: Id a -> a
  reveal = unId
  wrap :: a -> Id a
  wrap = Id

newtype Const b a = Const {unConst :: b}

instance Functor (Const c) where
  fmap :: (a -> b) -> Const c a -> Const c b
  fmap _ = Const . unConst

instance (Monoid m) => Applicative (Const m) where
  pure :: a -> Const m a
  pure _ = Const mempty
  (<*>) :: Const m (a -> b) -> Const m a -> Const m b
  Const mf <*> Const mx = Const (mf <> mx)

instance Coerce (Const a b) a where
  reveal :: Const a b -> a
  reveal = unConst
  wrap :: a -> Const a b
  wrap = Const

newtype M m a = Wrap {unWrap :: m a}

instance (Monad m) => Functor (M m) where
  fmap :: (Monad m) => (a -> b) -> M m a -> M m b
  fmap f = Wrap . fmap f . unWrap

instance (Monad m) => Applicative (M m) where
  pure :: a -> M m a
  pure = Wrap . pure
  (<*>) :: M m (a -> b) -> M m a -> M m b
  Wrap mf <*> Wrap mx = Wrap (mf >>= \f -> fmap f mx)

instance (Coerce (m a) c) => Coerce (M m a) c where
  reveal :: (Coerce (m a) c) => M m a -> c
  reveal = reveal . unWrap
  wrap :: (Coerce (m a) c) => c -> M m a
  wrap = Wrap . wrap

contentsBody :: a -> Const [a] b
contentsBody x = wrap [x]

contents :: (Traversable t) => t a -> Const [a] (t b)
contents = traverse contentsBody

run :: (Coerce b c, Traversable t) => (t a -> b) -> t a -> c
run program = reveal . program

runContents :: (Traversable t) => t a -> [a]
runContents = run contents

shapeBody :: a -> Id ()
shapeBody _ = wrap ()

shape :: (Traversable t) => t a -> Id (t ())
shape = traverse shapeBody

decompose :: (Traversable t) => t a -> (Id :*: Const [a]) (t ())
-- decompose = shape <:*:> contents
decompose = traverse (shapeBody <:*:> contentsBody)

reassembleBody :: () -> (M (State [a]) :.: M Maybe) a
-- reassembleBody :: () -> (State [a] :.: Maybe) a
reassembleBody = wrap . takeHead
  where
    takeHead :: () -> [a] -> (Maybe a, [a])
    takeHead _ = \case
      [] -> (Nothing, [])
      y : ys -> (Just y, ys)

reassemble :: (Traversable t) => t () -> (M (State [a]) :.: M Maybe) (t a)
reassemble = traverse reassembleBody

runReassemble :: (Traversable t) => (t (), [a]) -> Maybe (t a)
runReassemble = fst . uncurry (run reassemble)

reassembleBody' :: forall a. (State [a] :.: Maybe) a
-- reassembleBody' = undefined $ wrap @([a] -> (Maybe a, [a])) @((State [a] :.: Maybe) a) takeHead
reassembleBody' =
  -- wrap @((State [a] :.: Maybe) a) @([a] -> (Maybe a, [a])) takeHead
  -- Comp . state $ takeHead
  wrap takeHead
  where
    takeHead :: [a] -> (Maybe a, [a])
    takeHead = \case
      [] -> (Nothing, [])
      y : ys -> (Just y, ys)

collect :: (Traversable t, Applicative f) => (a -> f ()) -> (a -> b) -> t a -> f (t b)
collect f g = traverse (\x -> g x <$ f x)

loop :: (Traversable t) => (a -> b) -> t a -> M (State Int) (t b)
loop = collect (\_ -> Wrap (modify' (+ 1)))

disperse :: (Traversable t, Applicative f) => f b -> (a -> b -> c) -> t a -> f (t c)
disperse my f = traverse (\x -> f x <$> my)

label :: (Traversable t) => t a -> M (State Int) (t Int)
label = disperse (Wrap step) (\_ x -> x)

step :: State Int Int
step = do
  n <- get
  put (n + 1)
  pure n

reverseLabel :: (Traversable t) => t a -> State Int (t Int)
reverseLabel = ptraverse backwards (const step)

data ApplicativeAdapter f where
  ApplicativeAdapter ::
    (Applicative (g f)) =>
    (forall a. f a -> g f a) ->
    (forall a. g f a -> f a) ->
    ApplicativeAdapter f

ptraverse ::
  (Applicative f, Traversable t) =>
  ApplicativeAdapter f ->
  (a -> f b) ->
  t a ->
  f (t b)
ptraverse (ApplicativeAdapter insert retrieve) f = retrieve . traverse (insert . f)

newtype Forwards f a = Forwards {runForwards :: f a}

instance (Functor f) => Functor (Forwards f) where
  fmap :: (a -> b) -> Forwards f a -> Forwards f b
  fmap f = Forwards . fmap f . runForwards

instance (Applicative f) => Applicative (Forwards f) where
  pure :: a -> Forwards f a
  pure = Forwards . pure
  (<*>) :: Forwards f (a -> b) -> Forwards f a -> Forwards f b
  Forwards mf <*> Forwards mx = Forwards (mf <*> mx)

newtype Backwards f a = Backwards {runBackwards :: f a}

instance (Functor f) => Functor (Backwards f) where
  fmap :: (a -> b) -> Backwards f a -> Backwards f b
  fmap f = Backwards . fmap f . runBackwards

instance (Applicative f) => Applicative (Backwards f) where
  pure :: a -> Backwards f a
  pure = Backwards . pure
  (<*>) :: Backwards f (a -> b) -> Backwards f a -> Backwards f b
  Backwards mf <*> Backwards mx = Backwards ((&) <$> mx <*> mf)

forwards :: (Applicative f) => ApplicativeAdapter f
forwards = ApplicativeAdapter Forwards runForwards

backwards :: (Applicative f) => ApplicativeAdapter f
backwards = ApplicativeAdapter Backwards runBackwards

f' :: forall t f a b. (Applicative f, Traversable t) => (a -> f b) -> t a -> f (Maybe (t b))
f' f xs = fmap (curry runReassemble ys) (traverse f zs)
  where
    ys :: t ()
    zs :: [a]
    (ys, zs) = run decompose xs

type Count = Const (Max Int)

count :: a -> Count b
count _ = wrap 1

cciBody :: Char -> Count a
cciBody = count

cci :: String -> Count [a]
cci = traverse cciBody

test :: Bool -> Max Int
-- test = bool 0 1
test b = if b then 1 else 0

lciBody :: Char -> Count a
lciBody c = wrap (test (c == '\n'))

lci :: String -> Count [a]
lci = traverse lciBody

-- Why do I have to specify a of type kind?
wciBody :: Char -> (M (State Bool) :.: Count) a
wciBody ch =
    wrap (updateState ch)
  where
    updateState :: Char -> Bool -> (Max Int, Bool)
    updateState ch inWord =
      let result = not (isSpace ch)
       in (test (not inWord && result), result)
