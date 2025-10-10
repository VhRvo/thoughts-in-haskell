module CPS.Type.Type1 where

import Data.Text (Text)
import Data.Text qualified as T

data Type
  = Base Text
  | Arrow Type Type
  deriving (Eq, Ord)

instance Show Type where
  show :: Type -> String
  show = T.unpack . show'
    where
      show' :: Type -> Text
      show' =
        \case
          Base base -> base
          Arrow from to -> "(" <> show' from <> " -> " <> show' to <> ")"

infixr 9 -->

(-->) :: Type -> Type -> Type
(-->) = Arrow

a, b, c, d :: Type
a = Base "a"
b = Base "b"
c = Base "c"
d = Base "d"

cpsType :: Type -> Type
cpsType =
  \case
    Base base -> Base base
    Arrow from to -> Arrow (cpsType from) (cpsArrow to)

cpsArrow :: Type -> Type
cpsArrow =
  \case
    Base base ->
      let result = "r"
       in Arrow (Arrow (Base base) (Base result)) (Base result)
    Arrow from to -> Arrow (cpsType from) (cpsArrow to)

-- >>> cpsType a
-- >>> cpsType (a --> b)
-- >>> cpsType (a --> b --> c)
-- >>> cpsType ((a --> b) --> a --> b)
-- a
-- (a -> ((b -> rb) -> rb))
-- (a -> (b -> ((c -> rc) -> rc)))
-- ((a -> ((b -> rb) -> rb)) -> (a -> ((b -> rb) -> rb)))
