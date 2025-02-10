{-# LANGUAGE DeriveGeneric #-}

module ProductOfMonoidIsMonoid where

import Data.Coerce
import GHC.Generics (Generic, Generic1)
-- generic-deriving
import Generics.Deriving.Monoid

newtype XorFirst a = XorFirst {getXorFirst :: Maybe a}
  deriving
    ( Eq,
      Ord,
      Read,
      Show,
      Generic,
      Generic1,
      Functor,
      Applicative,
      Monad
    )

instance (Show a) => Semigroup (XorFirst a) where
  left <> right = case left of
    XorFirst Nothing -> right
    XorFirst (Just _) -> case right of
      XorFirst (Just _) -> error $ "only one side should be existed, here are two: " <> show left <> " and " <> show right
      XorFirst Nothing -> left

instance (Show a) => Monoid (XorFirst a) where
  mempty = XorFirst Nothing

data Monoids = Monoids
  { one :: XorFirst Int, -- Maybe Int
    two :: XorFirst Char -- Maybe Char
  }
  deriving (Generic, Show)

instance Semigroup Monoids where
  (<>) = mappenddefault

instance Monoid Monoids where
  mempty = memptydefault

main :: IO ()
main = do
  let m1 = Monoids (XorFirst (Just 1)) (coerce (Just 'c'))
  let m2 = mempty
  print (m1 <> m2)
  print (m2 <> m1)
  print (m1 <> m1)
