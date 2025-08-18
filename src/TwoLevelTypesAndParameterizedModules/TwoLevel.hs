{-# LANGUAGE PatternSynonyms #-}

module TwoLevelTypesAndParameterizedModules.TwoLevel where

-- import qualified Data.Text as T

import Control.Monad.ST
import Data.STRef
import Data.Text (Text)
import Prelude hiding (map, seq)
import Data.Foldable (traverse_)

-- data TypeExpr a
--   = MutVar (STRef a (Maybe (TypeExpr a)))
--   | GenVar Int
--   | OperatorType Text [ TypeExpr a ]

-- data TypeExpr r a
--   = MutVar (r (Maybe (TypeExpr r a)))
--   | GenVar Int
--   | OperatorType Text [ TypeExpr r a ]

-- data TypeExpr s r a
--   = MutVar (r (Maybe (TypeExpr s r a)))
--   | GenVar Int
--   | Structure (s (TypeExpr s r a))

-- data TypeExpr s r a
--   = MutVar (r (Maybe (TypeExpr s r a)))
--   | GenVar Int
--   | Structure (s (TypeExpr s r a))

-- Eliminating phantom type variable `a`.
-- data TypeExpr s r
--   = MutVar (r (Maybe (TypeExpr s r)))
--   | GenVar Int
--   | Structure (s (TypeExpr s r))

data Structure x = OperatorType Text [x]

data GenericTerm s r
  = MutVar (r (Maybe (GenericTerm s r)))
  | GenVar Int
  | Structure (s (GenericTerm s r))

type TypeExpr a = GenericTerm Structure (STRef a)

pattern POperatorType :: Text -> [GenericTerm Structure r] -> GenericTerm Structure r
pattern POperatorType operator types = Structure (OperatorType operator types)

operatorType :: Text -> [GenericTerm Structure r] -> GenericTerm Structure r
operatorType = POperatorType

class Structurable structure where
  map :: forall a b. (a -> b) -> structure a -> structure b
  acc :: forall a b. (a -> b -> b) -> structure a -> b -> b
  seq :: forall m a. (Monad m) => structure (m a) -> m (structure a)
  match :: forall a. structure a -> structure a -> Maybe [(a, a)]

instantiate ::
  forall s r. (Structurable s) => [GenericTerm s r] -> GenericTerm s r -> GenericTerm s r
instantiate types type' =
  case type' of
    MutVar _ ->
      type'
    GenVar index ->
      types !! index
    Structure structure ->
      Structure (map (instantiate types) structure)

instance Structurable Structure where
  map :: (a -> b) -> Structure a -> Structure b
  map f (OperatorType operator xs) = OperatorType operator (fmap f xs)
  acc :: (a -> b -> b) -> Structure a -> b -> b
  acc combine (OperatorType _ xs) initial = foldr combine initial xs
  seq :: (Monad m) => Structure (m a) -> m (Structure a)
  seq (OperatorType operator mxs) = fmap (OperatorType operator) (sequence mxs)
  match :: Structure a -> Structure a -> Maybe [(a, a)]
  match (OperatorType operator xs) (OperatorType operator' ys)
    | operator == operator' && length xs == length ys = Just (zip xs ys)
    | otherwise = Nothing

class EqRef ref where
  sameRef :: forall a. ref a -> ref a -> Bool

class (Monad m) => Reference ref m where
  newRef :: forall a. a -> m (ref a)
  readRef :: forall a. ref a -> m a
  writeRef :: forall a. ref a -> a -> m ()

prune ::
  forall ref m structure.
  (Reference ref m) =>
  GenericTerm structure ref ->
  m (GenericTerm structure ref)
prune expr =
  case expr of
    MutVar ptr ->
      readRef ptr >>= \case
        Nothing -> pure expr
        Just pointee -> do
          pointee <- prune pointee
          writeRef ptr (Just pointee)
          pure pointee
    _ -> pure expr

occursInType ::
  forall ref m structure.
  (EqRef ref, Reference ref m, Structurable structure) =>
  ref (Maybe (GenericTerm structure ref)) ->
  GenericTerm structure ref ->
  m Bool
occursInType ptr expr = do
  expr <- prune expr
  case expr of
    MutVar ptr' -> pure (sameRef ptr ptr')
    GenVar _ -> pure False
    Structure structure ->
      (\result -> acc (||) result False)
        <$> seq (map (occursInType ptr) structure)

unifyType ::
  forall ref structure m.
  (EqRef ref, Reference ref m, Structurable structure) =>
  GenericTerm structure ref ->
  GenericTerm structure ref ->
  m ()
unifyType lhs rhs = do
  lhs <- prune lhs
  rhs <- prune rhs
  case (lhs, rhs) of
    (MutVar ptr, MutVar ptr') ->
      if sameRef ptr ptr'
        then pure ()
        else writeRef ptr (Just rhs)
    (MutVar ptr, _) ->
      bind ptr rhs
    (_, MutVar ptr) ->
      bind ptr rhs
    (GenVar index, GenVar index') ->
      if index == index'
        then pure ()
        else error "different generic variables"
    (Structure lhs', Structure rhs') ->
      case match lhs' rhs' of
        Nothing -> error "different constructors"
        Just pairs -> traverse_ (uncurry unifyType) pairs
    _ -> error "different constructors"
  where
    bind ::
      ref (Maybe (GenericTerm structure ref)) ->
      GenericTerm structure ref ->
      m ()
    bind ptr expr = do
      occured <- occursInType ptr expr
      if occured
        then error "occured"
        else writeRef ptr (Just expr)

instance EqRef (STRef s) where
  sameRef :: STRef s a -> STRef s a -> Bool
  sameRef = (==)

instance Reference (STRef s) (ST s) where
  newRef :: a -> ST s (STRef s a)
  newRef = newSTRef
  readRef :: STRef s a -> ST s a
  readRef = readSTRef
  writeRef :: STRef s a -> a -> ST s ()
  writeRef = writeSTRef
