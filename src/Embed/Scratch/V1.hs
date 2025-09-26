{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use <$>" #-}
{-# HLINT ignore "Redundant $" #-}
module Embed.Scratch.V1 where

import Embed.Def

embed :: forall f a. (Functor f) => X f a -> Y f a
embed (XZ a) = YZ a
embed (XS x) = YS (fmap embed (distribute x))

-- embed (XS x) = collapse (embed @_ @(f a) x)

collapse :: forall f a. (Functor f) => Y f (f a) -> Y f a
collapse (YZ a) = YS (YZ <$> a)
collapse (YS x) = YS (collapse <$> x)

distribute :: forall f a. (Functor f) => X f (f a) -> f (X f a)
distribute (XZ a) = fmap XZ $ a
distribute (XS x) = fmap XS $ distribute @_ @(f a) x

-- distribute (XZ a) = XZ <$> a
-- distribute (XS x) = XS <$> distribute @_ @(f a) x
