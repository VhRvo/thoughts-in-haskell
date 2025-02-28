module TypeSafeObservableSharing.Class where

import Data.Kind (Type)

class MuRef a where
  type DeRef a :: Type -> Type
  mapDeRef ::
    (Applicative f) =>
    (a -> f u) ->
    a ->
    f (DeRef a u)
