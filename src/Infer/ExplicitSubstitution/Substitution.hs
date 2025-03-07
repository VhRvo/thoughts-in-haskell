module Infer.ExplicitSubstitution.Substitution where

import Data.Coerce
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Infer.Syntax
import Infer.TypeEnv

newtype Substitution
  = Substitution (Map Identifier Type)
  deriving (Show)

singleton :: Identifier -> Type -> Substitution
singleton = coerce (Map.singleton @_ @Type)

findWithDefault :: Type -> Identifier -> Substitution -> Type
findWithDefault = coerce (Map.findWithDefault @_ @Type)

fromList :: [(Identifier, Type)] -> Substitution
fromList = coerce (Map.fromList @_ @Type)

instance Semigroup Substitution where
  (<>) :: Substitution -> Substitution -> Substitution
  s1@(Substitution s1') <> Substitution s2 =
    Substitution (fmap (apply s1) s2 <> s1')

instance Monoid Substitution where
  mempty :: Substitution
  mempty = Substitution Map.empty

class Substitutable a where
  freeVars :: a -> Set Identifier
  apply :: Substitution -> a -> a

instance Substitutable Type where
  freeVars :: Type -> Set Identifier
  freeVars = \case
    TBoolean -> Set.empty
    TInteger -> Set.empty
    TVariable var -> Set.singleton var
    TArrow input output ->
      freeVars input <> freeVars output

  apply :: Substitution -> Type -> Type
  apply substitution = \case
    TBoolean -> TBoolean
    TInteger -> TInteger
    tVar@(TVariable var) ->
      findWithDefault tVar var substitution
    TArrow input output ->
      TArrow
        (apply substitution input)
        (apply substitution output)

instance Substitutable Scheme where
  freeVars :: Scheme -> Set Identifier
  freeVars (Scheme ids tpe) =
    Set.difference (Set.fromList ids) (freeVars tpe)

  apply :: Substitution -> Scheme -> Scheme
  apply substitution (Scheme ids tpe) =
    Scheme ids (apply substitution tpe)

instance Substitutable TypeEnv where
  freeVars :: TypeEnv -> Set Identifier
  freeVars (TypeEnv env) = foldr ((<>) . freeVars) Set.empty env

  apply :: Substitution -> TypeEnv -> TypeEnv
  apply substitution (TypeEnv env) = TypeEnv (apply substitution <$> env)
