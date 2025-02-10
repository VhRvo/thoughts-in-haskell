{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module GenericRandom.Demo where

import Data.Kind (Type)
import GHC.Generics
import Generic.Random
import Test.QuickCheck
import Test.QuickCheck (Arbitrary)
import Text.Pretty.Simple (pPrint)

data Tree a
  = Leaf a
  | Node (Tree a) (Tree a)
  deriving (Generic, Show)
--   deriving (Arbitrary) via (AndShrinking GenericArbitraryU' (Tree a))

--   deriving (Arbitrary) via (AndShrinking GenericArbitraryU (Tree a))
--   deriving (Arbitrary) via (AndShrinking (GenericArbitrary '[9, 8]) (Tree a))
--   deriving (Arbitrary) via ((GenericArbitraryRec '[9 :: W "Leaf", 8 :: W "Node"]) (Tree a))
--   deriving (Arbitrary) via ((GenericArbitraryRec (9 % 8 % ())) (Tree a))

instance (Arbitrary a, BaseCase (Tree a)) => Arbitrary (Tree a) where
  --   arbitrary = genericArbitrary (9 % 8 % ())
  --   arbitrary = genericArbitrary (W @('"Leaf") 9 % W @('"Node") 8 % ())
--   arbitrary = genericArbitrary ((9 :: W "Leaf") % (8 :: W "Node") % ())
  arbitrary = genericArbitrary' uniform
--   arbitrary = genericArbitraryRec ((9 :: W "Leaf") % (8 :: W "Node") % ())
  shrink = genericShrink
--   arbitrary = genericArbitrary ((Weight "Leaf" 9) % (8 :: W "Node") % ())

type family Weight symbol natural :: W symbol where
    Weight symbol natural = natural :: W symbol

-- instance Arbitrary a => Arbitrary (Tree a) where
--   arbitrary = genericArbitrary (9 % 8 % ())

main :: IO ()
main = do
  generate (arbitrary @(Tree Int)) >>= pPrint
  generate (arbitrary @(Tree Int)) >>= pPrint
  generate (arbitrary @(Tree Int)) >>= pPrint
  generate (arbitrary @(Tree Int)) >>= pPrint
