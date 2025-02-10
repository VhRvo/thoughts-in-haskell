module SmartView.FreeMonadPlus where


import Prelude hiding (head, tail)
import Control.Applicative
import Control.Monad

data FMP f x
  = FNil
  | ConsV x (FMP f x)
  | ConsF (f (FMP f x)) (FMP f x)
  | FMP f x :+ FMP f x
  | forall a. FMP f a :>>= (a -> FMP f x)

instance Functor f => Functor (FMP f) where
  fmap :: (a -> b) -> FMP f a -> FMP f b
  -- fmap = liftM
  fmap f = \case
    FNil -> FNil
    ConsV head tail -> ConsV (f head) (fmap f tail)
    ConsF head tail -> ConsF (fmap (fmap f) head) (fmap f tail)
    left :+ right -> fmap f left :+ fmap f right
    mx :>>= g -> mx :>>= (fmap f . g)

instance Functor f => Applicative (FMP f) where
  pure :: a -> FMP f a
  pure x = ConsV x FNil
  (<*>) :: FMP f (a -> b) -> FMP f a -> FMP f b
  (<*>) = ap
  -- ff <*> fx = case ff of
  --   FNil -> FNil
  --   ConsV headF tailF -> ConsV ()
  --   ConsF headF tailF -> undefined

instance Functor f => Monad (FMP f) where
  (>>=) :: Functor f => FMP f a -> (a -> FMP f b) -> FMP f b
  (>>=) = (:>>=)
  -- mx >>= f =
    -- case mx of
    --   FNil -> FNil
    --   ConsV head tail -> f head <|> (tail >>= f)
    --   ConsF head tail -> ConsF (fmap (>>= f) head) (tail >>= f)

instance Functor f => Alternative (FMP f) where
  empty :: FMP f a
  empty = FNil
  (<|>) :: FMP f a -> FMP f a -> FMP f a
  (<|>) = (:+)
  -- left <|> right = case left of
  --   FNil -> right
  --   ConsV head tail -> ConsV head (tail <|> right)
  --   ConsF head tail -> ConsF head (tail <|> right)
    -- ConsF head tail -> ConsF (fmap (<|> right) head) (tail <|> right)

data ViewM f x
  = FNilV
  | ConsVV x (FMP f x)
  | ConsFV (f (FMP f x)) (FMP f x)

viewM :: Functor f => FMP f x -> ViewM f x
viewM = \case
  FNil -> FNilV
  ConsV head tail -> ConsVV head tail
  ConsF head tail -> ConsFV head tail
  (m :+ n) :+ k -> viewM (m :+ (n :+ k))
  left :+ right -> case viewM left of
    FNilV -> viewM right
    ConsVV head tail -> ConsVV head (tail :+ right)
    ConsFV head tail -> ConsFV head (tail :+ right)
  (m :>>= f) :>>= g -> viewM (m :>>= (\x -> f x :>>= g))
  mx :>>= f -> case viewM mx of
    FNilV -> FNilV
    ConsVV head tail -> viewM (f head :+ (tail :>>= f))
    ConsFV head tail -> ConsFV (fmap (:>>= f) head) (tail :>>= f)




