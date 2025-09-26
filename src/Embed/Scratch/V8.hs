{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
module Embed.Scratch.V8 where

import Embed.Def

embed :: forall f a. (Functor f) => X f a -> Y f a
embed (XZ a) = YZ a
embed (XS x) = distributeEmbed x YS
  where
    -- distributeEmbed :: forall b. X f (f a) -> (f (Y f b) -> Y f a) -> Y f a
    distributeEmbed (XZ a) cont = cont (fmap YZ a)
    distributeEmbed (XS x) cont = undefined -- distributeEmbed x undefined -- (cont . fmap YS . fmap distribute)

distribute' :: forall f a. (Functor f) => Y f (f a) -> Y f a
distribute' = YS . distribute

distribute :: forall f a. (Functor f) => Y f (f a) -> f (Y f a)
distribute (YZ a) = fmap YZ . fmap id $ a
distribute (YS x) = fmap YS . fmap distribute $ x
