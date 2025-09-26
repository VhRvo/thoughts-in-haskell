{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Embed.Def where

data X f a = XZ a | XS (X f (f a))

data Y f a = YZ a | YS (f (Y f a))

deriving instance (Show a, Show (f (Y f a))) => Show (Y f a)

demoX :: X [] Int
demoX = XS (XS (XZ [[1], [2, 3], [4, 5, 6]]))

-- >>> YZ @[] (1 :: Int)
-- YZ 1

-- foldX :: forall f a r. (a -> r) ->
