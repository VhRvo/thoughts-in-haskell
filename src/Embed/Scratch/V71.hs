{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Functor law" #-}
{-# HLINT ignore "Redundant $" #-}
{-# HLINT ignore "Use <$>" #-}
module Embed.Scratch.V71 where

import Embed.Def

embed :: forall f a. (Functor f) => X f a -> Y f a
embed (XZ a) = YZ a
embed (XS x) = YS (distributeEmbed x)

distribute :: forall f a. (Functor f) => Y f (f a) -> f (Y f a)
distribute (YZ a) = fmap YZ $ a
distribute (YS x) = fmap (YS . distribute) $ x

distributeEmbed :: forall f a. (Functor f) => X f (f a) -> f (Y f a)
distributeEmbed (XZ a) = fmap YZ a
-- distributeEmbed (XS x) = fmap YS . fmap distribute $ distributeEmbed @_ @(f a) x
-- -- distributeEmbed (XS (XZ x)) = fmap YS . fmap distribute $ distributeEmbed @_ @(f a) (XZ x)
-- distributeEmbed (XS (XZ x)) = fmap YS . fmap distribute . fmap YZ $ x
distributeEmbed (XS (XZ x)) = fmap (YS . distribute) . fmap YZ $ x
-- -- distributeEmbed (XS (XS x)) = fmap YS . fmap distribute $ distributeEmbed @_ @(f a) (XS x)
-- \|> distributeEmbed (XS (XS x)) = fmap YS . fmap distribute . fmap YS . fmap distribute . distributeEmbed $ x
-- \|> distributeEmbed (XS (XS x)) = fmap YS . fmap (distribute . YS) . fmap distribute . distributeEmbed $ x
-- \|> distributeEmbed (XS (XS x)) = fmap YS . fmap (fmap (YS . distribute)) . fmap distribute . distributeEmbed $ x
-- distributeEmbed (XS (XS x)) = fmap (YS . fmap (YS . distribute)) . fmap distribute . distributeEmbed $ x
-- distributeEmbed (XS (XS (XZ x))) = fmap (YS . fmap (YS . distribute)) . fmap distribute . distributeEmbed $ XZ x
-- distributeEmbed (XS (XS (XZ x))) = fmap (YS . fmap (YS . distribute)) . fmap distribute . fmap YZ $ x
distributeEmbed (XS (XS (XZ x))) = fmap (YS . fmap (YS . distribute) . distribute) . fmap YZ $ x
-- distributeEmbed (XS (XS (XS x))) = fmap (YS . fmap (YS . distribute)) . fmap distribute . distributeEmbed $ XS x
-- distributeEmbed (XS (XS (XS x))) = fmap (YS . fmap (YS . distribute)) . fmap distribute . fmap YS . fmap distribute $ distributeEmbed x
-- distributeEmbed (XS (XS (XS x))) = fmap (YS . fmap (YS . distribute)) . fmap (distribute . YS) . fmap distribute $ distributeEmbed x
-- distributeEmbed (XS (XS (XS x))) = fmap (YS . fmap (YS . distribute) . (distribute . YS)) . fmap distribute $ distributeEmbed x
-- distributeEmbed (XS (XS (XS x))) = fmap (YS . fmap (YS . distribute) . fmap (YS . distribute)) . fmap distribute $ distributeEmbed x
-- distributeEmbed (XS (XS (XS x))) = fmap (YS . fmap (YS . distribute . YS . distribute)) . fmap distribute $ distributeEmbed x
-- distributeEmbed (XS (XS (XS x))) = fmap (YS . fmap (YS . fmap (YS . distribute) . distribute)) . fmap distribute $ distributeEmbed x
-- distributeEmbed (XS (XS (XS x))) = fmap (YS . fmap (YS . fmap (YS . distribute)) . fmap distribute) . fmap distribute $ distributeEmbed x
-- distributeEmbed (XS (XS (XS x))) = fmap (YS . fmap (YS . fmap (YS . distribute))) . fmap (fmap distribute) . fmap distribute $ distributeEmbed x
distributeEmbed (XS (XS (XS x))) = fmap (YS . fmap (YS . fmap (YS . distribute) . distribute) . distribute) $ distributeEmbed x
