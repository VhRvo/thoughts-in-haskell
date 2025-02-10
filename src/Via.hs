{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Via where

import Data.Monoid

newtype D a = D [a]
    deriving Semigroup via (Dual [a])
--   deriving newtype Semigroup -- (Semigroup via (Dual [a]))
--   deriving newtype (Monoid via (Dual [a]))

-- >>> D [1, 2, 3] <> D [4, 5, 6]

